
source("~/Dropbox/PoldrackLab/DevStudy_Analyses/code/helper_functions/rbind_all_columns.R")

sro_predict = function(x_df, x_var, y_df, y_var, shuffle_n =100){
  
  require(tidyverse)
  require(glmnet)
  
  for(i in 1:length(y_var)){
    
    cur_y_var = y_var[i]
    print(paste0('Running CV for y= ', cur_y_var, ' and x= ', x_var))
    
    y <- y_df %>% select(cur_y_var) %>% data.matrix()
    if("sub_id" %in% names(x_df)){
      x <- x_df %>% select(-sub_id) %>% data.matrix()
    } else{
      x <- x_df %>% data.matrix()
    }
    
    lambdas <- 10^seq(10, -2, by = -.1)
    
    cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, keep = TRUE)
    opt_prevals = cv_fit$fit.preval[,which(cv_fit$lambda == cv_fit$lambda.1se)]
    
    cur_fold_cors = data.frame(pre_vals = opt_prevals, fold_ids = cv_fit$foldid, act_ys = y[,])

    cur_fold_cors =  cur_fold_cors%>%
      group_by(fold_ids) %>%
      summarise(pred_cor = cor(act_ys, pre_vals)) %>%
      mutate(dv = cur_y_var,
             iv = x_var)
    
    shuffle_cors = NA
    for(j in 1:shuffle_n){
      shuffled_prevals = sample(opt_prevals, length(opt_prevals))
      shuffle_cors = c(shuffle_cors, cor(shuffled_prevals, y[,]))
    }
    shuffle_cors = shuffle_cors[!is.na(shuffle_cors)]
    
    cur_fold_cors$shuffle_95 = as.numeric(quantile(shuffle_cors, probs = c(0.95)))
    
    cur_betas = data.frame(t(as.matrix(coef(cv_fit,  s="lambda.1se"))))
    cur_cvm = cv_fit$cvm[which(lambdas == cv_fit$lambda.1se)]
    print(paste0('Cur CVM is ', cur_cvm))
    cur_out = data.frame(dv = cur_y_var, iv = x_var, cvm = cur_cvm)
    cur_out = cbind(cur_out, cur_betas)
    
    if(i == 1){
      out = cur_out
      fold_cors = cur_fold_cors
    } else{
      out = rbind.all.columns(out, cur_out)
      fold_cors = rbind.all.columns(fold_cors, cur_fold_cors)
    }
  }
  
  return(list(out=out, fold_cors=fold_cors))
}
