find_optimal_components = function(data, minc=1, maxc=50, fm="ml", verbose = FALSE, model = 'EFA'){
  
  require(tidyverse)
  require(psych)
  
  steps_since_best = 0 # count steps since last best metric.
  maxc = min(maxc, dim(data)[2])
  n_components = seq(minc,maxc)
  metrics = data.frame(comp=n_components, metric = NA)
  
  
  for(cur_c in n_components){
    
    if(verbose){
      print(cur_c)
    }
    
    if(model == "EFA"){
      out = fa(data, cur_c, rotate='oblimin', fm=fm, scores='tenBerge')
      
      if(is.null(out$BIC)){
        curr_metric = NA
      }
      else{
        curr_metric = out$BIC
      }
    }
    
    if(model == "PCA"){
      out = principal(data, nfactors=cur_c, rotate="oblimin")
      
      if(is.null(out$fit)){
        curr_metric = NA
      }
      else{
        curr_metric = out$fit^2
      }
    }
    
    metrics$metric[which(metrics$comp == cur_c)] = curr_metric
    
  }
  
  if(model == "EFA"){
    metrics = metrics %>% arrange(metric)
  }
  
  if(model == "PCA"){
    metrics = metrics %>% arrange(-metric)
  }
  
  return(metrics)
}