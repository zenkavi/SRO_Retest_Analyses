require(psych)
require(tidyverse)
require(RCurl)
helper_func_path = 'https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/'
if(from_gh){
  eval(parse(text = getURL(paste0(helper_func_path,'get_numeric_cols.R'), ssl.verifypeer = FALSE))) 
} else{
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/get_numeric_cols.R')
}


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

sqrt_const <- function(column, const){
  if(missing(const)|is.null(const)){
    col_min = min(column, na.rm=T)
    const = 1-col_min
  }
  column = column+const
  return(sqrt(column))
}

asin_count <- function(column, tpc){
  #tpc = total possible counts
  if(missing(tpc)|is.null(tpc)){
    tpc = max(column, na.rm=T)
  }
  column = sqrt(column/tpc)
  return(asin(column))
}

#transform: "log", "sqrt", "asin", "custom"
#if transform is custom: 
#mandatory argument "func" containing function to be applied must be provided
#optional argument "suffix" containing string to be added to transformed cols can be provided; else default string is .customTr

transform_remove_skew = function(data, columns=get_numeric_cols(df1=data, df2=data), threshold = 1, drop=FALSE, transform='log', verbose=TRUE, ...){
  
  tmp = as.data.frame(apply(data[,columns],2,skew))
  names(tmp) = c("skew")
  tmp$dv = row.names(tmp)
  tmp = tmp %>% 
    filter(abs(skew)>threshold)
  
  skewed_variables = tmp$dv
  skew_subset = data[, skewed_variables]
  
  if(length(tmp$dv[tmp$skew>0])>0){
    positive_subset = data.frame(data[,tmp$dv[tmp$skew>0]])
    names(positive_subset) = tmp$dv[tmp$skew>0]
  }
  if(length(tmp$dv[tmp$skew<0]>0)){
    negative_subset = data.frame(data[,tmp$dv[tmp$skew<0]])
    names(negative_subset) = tmp$dv[tmp$skew<0]
  }
  
  if(verbose){
    cat(rep('*', 40))
    cat('\n')
    cat(paste0('Using transformation:', transform))
    cat('\n')
  }
  
  if(exists('positive_subset')){ 
    if(!is.null(ncol(positive_subset)) & ncol(positive_subset)>0){
      
      if(transform == "log"){
        suffix = '.logTr'
        positive_subset = as.data.frame(apply(positive_subset, 2, pos_log))
      }
      
      if(transform == "sqrt"){
        suffix = '.sqrtTr'
        positive_subset = as.data.frame(apply(positive_subset, 2, sqrt_const, const=list(...)$const))
      }
      
      if(transform == "asin"){
        suffix = '.asinTr'
        positive_subset = as.data.frame(apply(positive_subset, 2, asin_count, tpc=list(...)$tpc))
      }
      
      if(transform == "custom"){
        if(!is.null(list(...)$suffix)){
          suffix = list(...)$suffix
        } else {
          suffix = '.customTr'
        }
        positive_subset = as.data.frame(apply(positive_subset, 2, list(...)$func))
      }
      
      successful_transforms = as.data.frame(apply(positive_subset, 2, skew))
      names(successful_transforms) = c('skew')
      successful_transforms$dv = row.names(successful_transforms)
      successful_transforms = successful_transforms %>% filter(abs(skew)<threshold)
      successful_transforms = successful_transforms$dv
      successful_transforms = positive_subset[,successful_transforms]
      dropped_vars = names(positive_subset) %w/o% names(successful_transforms)
      
      if(verbose){
        cat(rep('*', 40))
        cat('\n')
        cat(paste0(length(names(positive_subset)) ,' data positively skewed data were transformed:'))
        cat('\n')
        cat(names(positive_subset), sep = '\n')
        cat(rep('*', 40))
        cat('\n')
      }
      
      # replace transformed variables
      data = data[,-c(which(names(data) %in% names(positive_subset)))]
      
      if(drop == TRUE){
        names(successful_transforms) = paste0(names(successful_transforms), suffix)
        if(verbose){
          cat(rep('*', 40))
          cat('\n')
          cat(paste0('Dropping ', length(dropped_vars) ,' positively skewed data that could not be transformed successfully:'))
          cat('\n')
          cat(dropped_vars, sep = '\n')
          cat(rep('*', 40))
          cat('\n') 
        }
        data = cbind(data, successful_transforms)
      } else{
        names(positive_subset) = paste0(names(positive_subset), suffix)
        if(verbose){
          cat(rep('*', 40))
          cat('\n')
          cat(paste0(length(dropped_vars) ,' positively skewed data could not be transformed successfully:'))
          cat('\n')
          cat(dropped_vars, sep = '\n')
          cat(rep('*', 40))
          cat('\n') 
        }
        data = cbind(data, positive_subset)
      }
    } 
  }else{
   if(versbose){
     cat(rep('*', 40))
     cat('\n')
     cat('No positively skewed variables found.')
     cat(rep('*', 40))
     cat('\n')
   } 
  }
  
  
  if(exists('negative_subset')){
    if(!is.null(ncol(negative_subset)) & ncol(negative_subset)>0){
      
      if(transform == "log"){
        suffix = '.ReflogTr'
        negative_subset = as.data.frame(apply(negative_subset, 2, neg_log))
      }
      
      if(transform == "sqrt"){
        suffix = '.sqrtTr'
        negative_subset = as.data.frame(apply(negative_subset, 2, sqrt_const, const=list(...)$const))
      }
      
      if(transform == "asin"){
        suffix = '.asinTr'
        negative_subset = as.data.frame(apply(negative_subset, 2, asin_count, tpc=list(...)$tpc))
      }
      if(transform == "custom"){
        if(!is.null(list(...)$suffix)){
          suffix = list(...)$suffix
        } else {
          suffix = '.customTr'
        }
        negative_subset = as.data.frame(apply(negative_subset, 2, list(...)$func))
      }
      
      successful_transforms = as.data.frame(apply(negative_subset, 2, skew))
      names(successful_transforms) = c('skew')
      successful_transforms$dv = row.names(successful_transforms)
      successful_transforms = successful_transforms %>% filter(abs(skew)<1)
      successful_transforms = successful_transforms$dv
      successful_transforms = negative_subset[,successful_transforms]
      dropped_vars = names(negative_subset) %w/o% names(successful_transforms)
      
      if(verbose){
        cat(rep('*', 40))
        cat('\n')
        cat(paste0(length(names(negative_subset)) ,' data negatively skewed data were transformed:'))
        cat('\n')
        cat(names(negative_subset), sep = '\n')
        cat(rep('*', 40))
        cat('\n')
      }
      
      # replace transformed variables
      data = data[,-c(which(names(data) %in% names(negative_subset)))]
      if(drop == TRUE){
        names(successful_transforms) = paste0(names(successful_transforms), suffix)
        if(verbose){
          cat(rep('*', 40))
          cat('\n')
          cat(paste0('Dropping ', length(dropped_vars) ,' negatively skewed data that could not be transformed successfully:'))
          cat('\n')
          cat(dropped_vars, sep = '\n')
          cat(rep('*', 40))
          cat('\n') 
        }
        data = cbind(data, successful_transforms)
      } else{
        names(negative_subset) = paste0(names(negative_subset), suffix)
        if(verbose){
          cat(rep('*', 40))
          cat('\n')
          cat(paste0(length(dropped_vars) ,' negatively skewed data could not be transformed successfully:'))
          cat('\n')
          cat(dropped_vars, sep = '\n')
          cat(rep('*', 40))
          cat('\n') 
        }
        data = cbind(data, negative_subset)
      } 
    } 
  } else{
    if(verbose){
      cat(rep('*', 40))
      cat('\n')
      cat('No negatively skewed variables found.')
      cat(rep('*', 40))
      cat('\n') 
    }
  }
  
  return(data)
}
