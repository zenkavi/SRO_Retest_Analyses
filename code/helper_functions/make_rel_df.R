if(!exists('helper_func_path')){
  if(from_gh){
    library(RCurl)
    helper_func_path = 'https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/'
  }else{
    helper_func_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/'
  }
}

if(!exists('get_retest_stats')){
  if(from_gh){
    library(RCurl)
    eval(parse(text = getURL(paste0(helper_func_path,'get_retest_stats.R'), ssl.verifypeer = FALSE)))
  }else{
    source(paste0(helper_func_path, 'get_retest_stats.R'))
  }
}

if(!exists('get_numeric_cols')){
  if(from_gh){
    library(RCurl)
    eval(parse(text = getURL(paste0(helper_func_path,'get_numeric_cols.R'), ssl.verifypeer = FALSE)))
  }else{
    source(paste0(helper_func_path, 'get_numeric_cols.R'))
  }
}

make_rel_df = function(t1_df, t2_df, metrics, vars = NA, sample="full"){

  if(is.na(vars)){
    numeric_cols = get_numeric_cols(df1 = t1_df, df2 = t2_df)
  }else{
      numeric_cols = vars
    }

  rel_df_cols = metrics
  if('var_breakdown' %in% metrics){
    rel_df_cols = rel_df_cols[rel_df_cols != 'var_breakdown']
    rel_df_cols = c(rel_df_cols, 'var_subs', 'var_ind', 'var_resid')
  }
  rel_df_cols = c(rel_df_cols, 'dv')

  rel_df = as.data.frame(matrix(ncol = length(rel_df_cols)))

  names(rel_df) = rel_df_cols

  for(i in 1:length(numeric_cols)){

    cur_dv = numeric_cols[i]

    tmp = get_retest_stats(cur_dv, metric = metrics, t1_df = t1_df, t2_df = t2_df, sample=sample)

    if(nrow(tmp) == 0){
      tmp[1,]=NA
      tmp$dv = NA
    } else {
      tmp$dv = cur_dv
    }

    rel_df = rbind(rel_df, tmp)

  }

  rel_df= rel_df[-which(is.na(rel_df$dv)),]

  return(rel_df)
}
