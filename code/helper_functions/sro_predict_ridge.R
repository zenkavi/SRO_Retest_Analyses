
source("~/Dropbox/PoldrackLab/DevStudy_Analyses/code/helper_functions/rbind_all_columns.R")

sro_predict = function(x_df, x_var, y_df, y_var){
  
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
    
    tryCatch(
      cur_fold_cors = data.frame(pre_vals = cv_fit$fit.preval[,which(lambdas == cv_fit$lambda.1se)], fold_ids = cv_fit$foldid, act_ys = y[,]) %>%
        group_by(fold_ids) %>%
        summarise(pred_cor = cor(act_ys, pre_vals), 
                  shuffle_cor = cor(sample(act_ys, length(act_ys)), pre_vals)) %>%
        mutate(dv = cur_y_var,
               iv = x_var),
      error = function(e) {cv_fit}
    )
    
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
