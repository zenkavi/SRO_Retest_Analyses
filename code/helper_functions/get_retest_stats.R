require(psych)
require(tidyr)
library(RCurl)

if(!exists('match_t1_t2')){
  if(from_gh){
    eval(parse(text = getURL('https://raw.githubusercontent.com/zenkavi/SRO_DDM_Analyses/master/code/helper_functions/match_t1_t2.R', ssl.verifypeer = FALSE)))
  }else{
    source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/match_t1_t2.R')
  }
}

get_retest_stats = function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', metric = c('spearman', 'pearson', 'var_breakdown', 'partial_eta', 'sem', 'icc1.1','icc1.k','icc2.1', 'icc2.k','icc3.1','icc3.k')){
  
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide')
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide', sample='bootstrap')
  }
  
  out_cols = length(metric)
  out_names = metric
  
  if('var_breakdown' %in% metric){
    out_cols = out_cols+2
    out_names = out_names[out_names != 'var_breakdown']
    out_names = c(out_names, 'var_subs', 'var_ind', 'var_resid')
  }

  out = data.frame(matrix(ncol=out_cols))
  names(out) = out_names
  
  if('spearman' %in% metric){
    out$spearman = cor(df$`1`, df$`2`, method='spearman')
  }
  
  if('pearson' %in% metric){
    out$pearson = cor(df$`1`, df$`2`, method='pearson')
  }
  
  if(sum(c('icc', 'var_breakdown','icc1.1','icc1.k','icc2.1', 'icc2.k','icc3.1','icc3.k') %in% metric) > 0){
    icc = ICC(df %>% select(-dv, -sub_id), lmer=FALSE)
    
    if('icc1.1' %in% metric){
      out$icc1.1 = icc$results['Single_raters_absolute', 'ICC']
    }
    
    if('icc1.k' %in% metric){
      out$icc1.k = icc$results['Average_raters_absolute', 'ICC']
    }
    
    if('icc2.1' %in% metric){
      out$icc2.1 = icc$results['Single_random_raters', 'ICC']
    }
    
    if('icc2.k' %in% metric){
      out$icc2.k = icc$results['Average_random_raters', 'ICC']
    }
    
    if('icc3.1' %in% metric){
      out$icc3.1 = icc$results['Single_fixed_raters', 'ICC']
    }
    
    if('icc3.k' %in% metric){
      out$icc3.k = icc$results['Average_fixed_raters', 'ICC']
    }
    
    if('var_breakdown' %in% metric){
      out$var_subs = icc$summary[[1]][1,'Mean Sq']
      out$var_ind = icc$summary[[1]][2,'Mean Sq']
      out$var_resid = icc$summary[[1]][3,'Mean Sq']
    }
    
  }
  
  if('partial_eta' %in% metric | 'sem' %in% metric){
    
    mod = summary(aov(score~Error(sub_id)+time, df %>% gather(time, score, -dv, -sub_id)))
    
    if('partial_eta' %in% metric){
      ss_time = as.data.frame(unlist(mod$`Error: Within`))['Sum Sq1',]
      ss_error = as.data.frame(unlist(mod$`Error: Within`))['Sum Sq2',]
      out$partial_eta = ss_time/(ss_time+ss_error)
    }
    
    if('sem' %in% metric){
      ms_error = as.data.frame(unlist(mod$`Error: Within`))['Mean Sq2',]
      out$sem = sqrt(ms_error)
    }
    
  }
  return(out %>% drop_na())
  
}