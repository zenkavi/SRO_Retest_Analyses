get_fold_cors = function(model, shuffle=FALSE, shuffle_n = 100){
  
  require(tidyverse)
  
  out = data.frame(fold=NA, R =NA)
  
  x_var = model$trainingData[1]
  y_var = model$trainingData[,2]
  
  for(i in 1:length(model$control$indexOut)){
    
    indices = model$control$indexOut[[i]]
    train_df = data.frame(y = y_var[-indices], x = x_var[-indices,])
    test_df = data.frame(y = y_var[indices], x = x_var[indices,])
    m = lm(y ~ x, train_df)
    test_df$pred = predict(m, test_df)
    R = with(test_df,cor(pred, y))
    
    tmp = data.frame(fold=i, R=R)
    
    out = out %>% bind_rows(tmp)
    
    if(i == 1){
      all_preds = test_df
    }
    else{
      all_preds = rbind(all_preds, test_df)
    }
  }
  
  out = out[-1,]
  
  out$all_folds_r = with(all_preds, cor(pred, y))
  out$all_folds_r2 = out$all_folds_r^2
  out$all_folds_rmse = sqrt(mean((all_preds$y - all_preds$pred)^2))
  
  if(shuffle){
    feature_out = data.frame(shuffle_r=NA, shuffle_r2=NA, shuffle_rmse = NA)
    for(i in 1:shuffle_n){
      shuffled_df = all_preds
      shuffled_df[,"x"] = sample(all_preds[,"x"], length(all_preds[,"x"]))
      names(shuffled_df)[which(names(shuffled_df)=="x")] = names(x_var)
      shuffle_preds = predict(model, shuffled_df)
      shuffle_out = data.frame(shuffle_r = cor(shuffle_preds, shuffled_df$y),
                               shuffle_r2 = cor(shuffle_preds, shuffled_df$y)^2,
                               shuffle_rmse = sqrt(mean((shuffle_preds - shuffled_df$y)^2)))
      feature_out = rbind(feature_out, shuffle_out)
    }
    feature_out = feature_out[-1,]
  }
  
  if(shuffle){
    out$shuffle_mean_r = mean(feature_out$shuffle_r)
    out$shuffle_sem_r = sd(feature_out$shuffle_r)/sqrt(shuffle_n)
    out$shuffle_mean_r2 = mean(feature_out$shuffle_r2)
    out$shuffle_sem_r2 = sd(feature_out$shuffle_r2)/sqrt(shuffle_n)
    out$shuffle_mean_rmse = mean(feature_out$shuffle_rmse)
    out$shuffle_sem_rmse = sd(feature_out$shuffle_rmse)/sqrt(shuffle_n)
  }
  
  
  return(out)
}

sro_predict = function(x_df, y_df, cv_folds = 10, m_type = "lm", shuffle= FALSE, shuffle_n = 100){

  require(tidyverse)

  out = data.frame(dv=NA, iv=NA, Rsquared=NA, RsquaredSD=NA, RMSE=NA, RMSESD=NA)
  
  if(shuffle== FALSE){
    fold_cors = data.frame(dv=NA, iv=NA, fold=NA, R = NA, all_folds_r = NA, all_folds_r2 = NA, all_folds_rmse = NA)
  }
  else{
    fold_cors = data.frame(dv=NA, iv=NA, fold=NA, R = NA, all_folds_r = NA, all_folds_r2 = NA, all_folds_rmse = NA, shuffle_mean_r = NA, shuffle_sem_r = NA,  shuffle_mean_r2 = NA, shuffle_sem_r2 = NA,  shuffle_mean_rmse = NA, shuffle_sem_rmse = NA)
  }
  
  if("sub_id" %in% names(x_df)){
    x_s = names(x_df)[-which(names(x_df)=="sub_id")]
  }
  else{
    x_s = names(x_df)
  }
  if("sub_id" %in% names(y_df)){
    y_s = names(y_df)[-which(names(y_df)=="sub_id")]
  }
  else{
    y_s = names(y_df)

  }

  for(i in y_s){
    for(j in x_s){

      x = x_df%>%select(j)
      y = y_df[,i]

      print(paste0('Running CV for y= ', i, ' and x= ', j))

      model = train(x,y,
                    method=m_type,
                    trControl = trainControl(method="cv", number=cv_folds),
                    na.action = na.exclude)

      tmp = data.frame(dv = i, iv = j, Rsquared = model$results$Rsquared, RsquaredSD = model$results$RsquaredSD, RMSE = model$results$RMSE, RMSESD = model$results$RMSESD)

      out = rbind(out, tmp)
      
      tmp_cors = get_fold_cors(model, shuffle = shuffle, shuffle_n = shuffle_n)
      tmp_cors$dv = i
      tmp_cors$iv = j
      
      if(shuffle == FALSE){
        tmp_cors = tmp_cors %>% select(dv, iv, fold, R, all_folds_r, all_folds_r2, all_folds_rmse)
      }
      else{
        tmp_cors = tmp_cors %>% select(dv, iv, fold, R, all_folds_r, all_folds_r2, all_folds_rmse, shuffle_mean_r, shuffle_sem_r,  shuffle_mean_r2, shuffle_sem_r2,  shuffle_mean_rmse, shuffle_sem_rmse)
      }
      
      fold_cors = rbind(fold_cors, tmp_cors)

      print("Done with CV and fold cor's")
    }
  }

  out = out[-1,]
  fold_cors = fold_cors[-1,]
  out$all_folds_r = unique(fold_cors$all_folds_r)
  out$all_folds_r2 = unique(fold_cors$all_folds_r2)
  out$all_folds_rmse = unique(fold_cors$all_folds_rmse)
  
  if(shuffle){
    out$shuffle_mean_r = unique(fold_cors$shuffle_mean_r)
    out$shuffle_sem_r = unique(fold_cors$shuffle_sem_r)
    out$shuffle_mean_r2 = unique(fold_cors$shuffle_mean_r2)
    out$shuffle_sem_r2 = unique(fold_cors$shuffle_sem_r2)
    out$shuffle_mean_rmse = unique(fold_cors$shuffle_mean_rmse)
    out$shuffle_sem_rmse = unique(fold_cors$shuffle_sem_rmse)
  }
  
  return(list(out=out, fold_cors=fold_cors))
}
