get_fold_cors = function(model){
  
  out = data.frame(fold=NA, R =NA)
  
  x = model$trainingData[1]
  y = model$trainingData[,2]
  
  for(i in 1:length(model$control$indexOut)){
    
    indices = model$control$indexOut[[i]]
    train_df = data.frame(y = y[-indices], x = x[-indices,])
    test_df = data.frame(y = y[indices], x = x[indices,])
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
  return(out)
}

sro_predict = function(x_df, y_df, cv_folds = 10, m_type = "lm"){

  require(tidyverse)

  out = data.frame(dv=NA, iv=NA, Rsquared=NA, RsquaredSD=NA, RMSE=NA, RMSESD=NA)
  fold_cors = data.frame(dv=NA, iv=NA, fold=NA, R = NA, all_folds_r = NA, all_folds_r2 = NA, all_folds_rmse = NA)
  
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
      
      tmp_cors = get_fold_cors(model)
      tmp_cors$dv = i
      tmp_cors$iv = j
      tmp_cors = tmp_cors %>% select(dv, iv, fold, R, all_folds_r, all_folds_r2, all_folds_rmse)
      
      fold_cors = rbind(fold_cors, tmp_cors)

      print("Done with CV and fold cor's")
    }
  }

  out = out[-1,]
  fold_cors = fold_cors[-1,]
  out$all_folds_r = unique(fold_cors$all_folds_r)
  out$all_folds_r2 = unique(fold_cors$all_folds_r2)
  out$all_folds_rmse = unique(fold_cors$all_folds_rmse)
  return(list(out=out, fold_cors=fold_cors))
}
