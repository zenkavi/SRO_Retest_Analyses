residualize_baseline = function(df, baseline_vars = c("Age", "Sex")){
  require(tidyverse)

  X = df %>%
    select(one_of(baseline_vars))
  
  Y = df %>%
    select(-one_of(c("X", "sub_id", "subj_id", baseline_vars)))
  
  
  for(v in names(Y)){
    tmp = cbind(X, Y %>% select(v))
    m = lm(as.formula(paste(v,' ~ .')), tmp)
    tmp$v_pred = tmp[,v] - predict(m)
    Y[,v] = tmp$v_pred
  }
  
  return(Y)
  
}