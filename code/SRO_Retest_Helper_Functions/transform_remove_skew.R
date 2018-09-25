"%w/o%" <- function(x, y) x[!x %in% y]

pos_log <- function(column){
  col_min = min(column, na.rm=T)
  a = 1-col_min
  column = column+a
  return(log(column))
}

neg_log <- function(column){
  col_max = max(column, na.rm=T)
  column = col_max+1-column
  return(log(column))
}

transform_remove_skew = function(data, columns, threshold = 1, drop=FALSE){
  
  tmp = as.data.frame(apply(data[,columns],2,skew))
  names(tmp) = c("skew")
  tmp$dv = row.names(tmp)
  tmp = tmp %>% 
    filter(abs(skew)>threshold)
  
  skewed_variables = tmp$dv
  skew_subset = data[, skewed_variables]
  positive_subset = data[,tmp$dv[tmp$skew>0]]
  negative_subset = data[,tmp$dv[tmp$skew<0]]
  
  # transform variables
  # log transform for positive skew
  # positive_subset = log(positive_subset)
  # positive_subset = pos_log(positive_subset) #slight divergence from original code
  positive_subset = as.data.frame(apply(positive_subset, 2, pos_log))
  successful_transforms = as.data.frame(apply(positive_subset, 2, skew))
  names(successful_transforms) = c('skew')
  successful_transforms$dv = row.names(successful_transforms)
  successful_transforms = successful_transforms %>% filter(abs(skew)<threshold)
  successful_transforms = successful_transforms$dv
  successful_transforms = positive_subset[,successful_transforms]
  dropped_vars = names(positive_subset) %w/o% names(successful_transforms)
  
  cat(rep('*', 40))
  cat('\n')
  cat(paste0(length(names(positive_subset)) ,' data positively skewed data were transformed:'))
  cat('\n')
  cat(names(positive_subset), sep = '\n')
  cat(rep('*', 40))
  cat('\n')
  
  # replace transformed variables
  data = data[,-c(which(names(data) %in% names(positive_subset)))]
  
  if(drop == TRUE){
    names(successful_transforms) = paste0(names(successful_transforms), '.logTr')
    cat(rep('*', 40))
    cat('\n')
    cat(paste0('Dropping ', length(dropped_vars) ,' positively skewed data that could not be transformed successfully:'))
    cat('\n')
    cat(dropped_vars, sep = '\n')
    cat(rep('*', 40))
    cat('\n')
    data = cbind(data, successful_transforms)
  }
  else{
    names(positive_subset) = paste0(names(positive_subset), '.logTr')
    cat(rep('*', 40))
    cat('\n')
    cat(paste0(length(dropped_vars) ,' positively skewed data could not be transformed successfully:'))
    cat('\n')
    cat(dropped_vars, sep = '\n')
    cat(rep('*', 40))
    cat('\n')
    data = cbind(data, positive_subset)
  }
  
  
  # reflected log transform for negative skew      
  negative_subset = as.data.frame(apply(negative_subset, 2, neg_log))
  successful_transforms = as.data.frame(apply(negative_subset, 2, skew))
  names(successful_transforms) = c('skew')
  successful_transforms$dv = row.names(successful_transforms)
  successful_transforms = successful_transforms %>% filter(abs(skew)<1)
  successful_transforms = successful_transforms$dv
  successful_transforms = negative_subset[,successful_transforms]
  dropped_vars = names(negative_subset) %w/o% names(successful_transforms)
  
  cat(rep('*', 40))
  cat('\n')
  cat(paste0(length(names(negative_subset)) ,' data negatively skewed data were transformed:'))
  cat('\n')
  cat(names(negative_subset), sep = '\n')
  cat(rep('*', 40))
  cat('\n')
  
  # replace transformed variables
  data = data[,-c(which(names(data) %in% names(negative_subset)))]
  if(drop == TRUE){
    names(successful_transforms) = paste0(names(successful_transforms), '.ReflogTr')
    cat(rep('*', 40))
    cat('\n')
    cat(paste0('Dropping ', length(dropped_vars) ,' negatively skewed data that could not be transformed successfully:'))
    cat('\n')
    cat(dropped_vars, sep = '\n')
    cat(rep('*', 40))
    cat('\n')
    data = cbind(data, successful_transforms)
  }
  else{
    names(negative_subset) = paste0(names(negative_subset), '.ReflogTr')
    cat(rep('*', 40))
    cat('\n')
    cat(paste0(length(dropped_vars) ,' negatively skewed data could not be transformed successfully:'))
    cat('\n')
    cat(dropped_vars, sep = '\n')
    cat(rep('*', 40))
    cat('\n')
    data = cbind(data, negative_subset)
  }
  
  return(data)
}