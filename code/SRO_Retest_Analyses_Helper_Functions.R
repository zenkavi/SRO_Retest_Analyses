library(tidyverse)
library(gridExtra)
library(lme4)
library(GGally)
library(jsonlite)
library(psych)
library(rmarkdown)
library(psych)
library(stringr)
library(plotly)
library(DT)
library(sjPlot)
library(RecordLinkage)
library(MCMCglmm)

render_this <- function(){rmarkdown::render('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/SRO_Retest_Analyses.Rmd', output_dir = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/reports', html_notebook(toc = T, toc_float = T, toc_depth = 2, code_folding = 'hide'))}

options(scipen = 1, digits = 4)

helper_func_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/SRO_Retest_Helper_Functions/'

source(paste0(helper_func_path, 'g_legend.R'))
source(paste0(helper_func_path, 'sem.R'))
source(paste0(helper_func_path, 'trim.R'))
source(paste0(helper_func_path, 'match_t1_t2.R'))




get_spearman = function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
  
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide')
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide', sample='bootstrap', sample_vec = sample_vec)
  }
  
  rho = cor(df$`1`, df$`2`, method='spearman')
  
  return(rho)
}

get_icc <- function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide')
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide', sample='bootstrap', sample_vec = sample_vec)
  }
  
  df = df %>% select(-dv, -sub_id)
  icc = ICC(df)
  icc_3k = icc$results['Average_fixed_raters', 'ICC']
  return(icc_3k)
}

get_pearson = function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
  
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide')
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide', sample='bootstrap', sample_vec = sample_vec)
  }
  
  r = cor(df$`1`, df$`2`, method='pearson')
  
  return(r)
}

get_var_breakdown <- function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide')
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide', sample='bootstrap', sample_vec = sample_vec)
  }
  
  df = df %>% select(-dv, -sub_id)
  icc = ICC(df)
  var_breakdown = data.frame(subs = icc$summary[[1]][1,'Mean Sq'],
                             ind = icc$summary[[1]][2,'Mean Sq'],
                             resid = icc$summary[[1]][3,'Mean Sq'])
  return(var_breakdown)
}

get_partial_eta <- function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var)
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, sample='bootstrap', sample_vec = sample_vec)
  }
  
  mod = summary(aov(score~Error(sub_id)+time, df))
  ss_time = as.data.frame(unlist(mod$`Error: Within`))['Sum Sq1',]
  ss_error = as.data.frame(unlist(mod$`Error: Within`))['Sum Sq2',]
  partial_eta = ss_time/(ss_time+ss_error)
  return(partial_eta)
}

get_sem <- function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var)
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, sample='bootstrap', sample_vec = sample_vec)
  }
  mod = summary(aov(score~Error(sub_id)+time, df))
  ms_error = as.data.frame(unlist(mod$`Error: Within`))['Mean Sq2',]
  sem = sqrt(ms_error)
  return(sem)
}