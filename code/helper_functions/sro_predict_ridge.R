
sro_predict = function(x_df, x_var, y_df, y_var){
  
  require(tidyverse)
  require(glmnet)
  
  for(i in 1:length(y_var)){
    
    cur_y_var = y_var[i]
    print(paste0('Running CV for y= ', cur_y_var, ' and x= ', x_var))
    
    y <- y_df %>% select(cur_y_var) %>% data.matrix()
    x <- x_df %>% select(-sub_id) %>% data.matrix()
    lambdas <- 10^seq(10, -2, by = -.1)
    
    cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, keep = TRUE)
    
    fold_cors = data.frame(pre_vals = cv_fit$fit.preval[,which(lambdas == cv_fit$lambda.min)], fold_ids = cv_fit$foldid, act_ys = y[,]) %>%
      group_by(fold_ids) %>%
      summarise(pred_cor = cor(act_ys, pre_vals), 
                shuffle_cor = cor(sample(act_ys, length(act_ys)), pre_vals)) %>%
      mutate(dv = cur_y_var,
             iv = x_var)
    
    for(j in 1:length(folds)){
      cur_fold = folds[j] 
      actual_ys = y[cv_fit$foldid == cur_fold]
      actual_xs = x_df[cv_fit$foldid == cur_fold,] %>% select(-sub_id) %>% data.matrix()
      pred_ys = predict(cv_fit$glmnet.fit, s = cv_fit$lambda.min, newx = actual_xs)
      cur_cor = cor(actual_ys, pred_ys)
      shuffle_ys = sample(pred_ys, length(pred_ys))
      shuffle_cor = cor(actual_ys, shuffle_ys)
      cur_row = data.frame(dv=cur_y_var, iv=x_var, fold=cur_fold, act_pred_cor=cur_cor, shuffle_r=shuffle_cor)
      if(i==1 & j == 1){
        fold_cors = cur_row
      } else{
        fold_cors = rbind(fold_cors, cur_row)
      }
    }
    
    cur_betas = cv_fit$glmnet.fit$beta[,which(lambdas == cv_fit$lambda.min)]
    cur_betas = data.frame(as.list(cur_betas))
    cur_cvm = cv_fit$cvm[which(lambdas == cv_fit$lambda.min)]
    print(paste0('Cur CVM is ', cur_cvm))
    
    cur_out = data.frame(dv = cur_y_var, iv = x_var, cvm = cur_cvm)
    cur_out = cbind(cur_out, cur_betas)
    if(i == 1){
      out = cur_out
    } else{
      out = rbind(out, cur_out)
    }
  }
  
  return(list(out=out, fold_cors=fold_cors))
}

data.frame(pre_vals = cv_fit$fit.preval[,which(lambdas == cv_fit$lambda.min)], fold_ids = cv_fit$foldid, act_ys = y[,]) %>%
  group_by(fold_ids) %>%
  summarise(pred_cor = cor(act_ys, pre_vals))
