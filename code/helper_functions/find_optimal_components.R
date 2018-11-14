find_optimal_components = function(data, minc=1, maxc=50, fm="ml", verbose = FALSE){
  
  require(tidyverse)
  require(psych)
  
  steps_since_best = 0 # count steps since last best metric.
  maxc = min(maxc, dim(data)[2])
  n_components = seq(minc,maxc)
  metrics = data.frame(comp=n_components, BIC = NA)
  best_metric = NA
  best_c = 0
  
  for(cur_c in n_components){
    
    if(verbose){
      print(cur_c)
    }
    
    out = fa(data, cur_c, rotate='oblimin', fm=fm, scores='tenBerge')
    
    if(is.null(out$BIC)){
      curr_metric = NA
    }
    else{
      curr_metric = out$BIC
    }
    
    metrics$BIC[which(metrics$comp == cur_c)] = curr_metric
  }
  
  metrics = metrics %>% arrange(BIC)
  
  return(metrics)
}