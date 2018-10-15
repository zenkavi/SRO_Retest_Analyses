get_retest_stats = function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec, metric = c('spearman', 'icc', 'pearson', 'var_breakdown', 'partial_eta', 'sem')){
  
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide')
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide', sample='bootstrap', sample_vec = sample_vec)
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
  
  if('icc' %in% metric | 'var_breakdown' %in% metric){
    df = df %>% select(-dv, -sub_id)
    icc = ICC(df)
    
    if('icc' %in% metric){
      out$icc = icc$results['Average_fixed_raters', 'ICC']
    }
    
    if('var_breakdown' %in% metric){
      out$var_subs = icc$summary[[1]][1,'Mean Sq']
      out$var_ind = icc$summary[[1]][2,'Mean Sq']
      out$var_resid = icc$summary[[1]][3,'Mean Sq']
    }
    
  }
  
  if('partial_eta' %in% metric | 'sem' %in% metric){
    
    mod = summary(aov(score~Error(sub_id)+time, df))
    
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