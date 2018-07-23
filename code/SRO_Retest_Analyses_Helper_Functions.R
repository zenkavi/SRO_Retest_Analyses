library(dplyr)
library(tidyr)
library(ggplot2)
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

sem <- function(x) {sd(x, na.rm=T) / sqrt(length(x))}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

match_t1_t2 <- function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', format = "long", sample = 'full', sample_vec){
  
  if(sample == 'full'){
    df = merge(t1_df[,c(merge_var, dv_var)], t2_df[,c(merge_var, dv_var)], by = merge_var) 
  }
  else{
    df = merge(t1_df[t1_df[,merge_var] %in% sample_vec, c(merge_var, dv_var)], t2_df[t2_df[,merge_var] %in% sample_vec, c(merge_var, dv_var)],
               by=merge_var)
  }
  
  df = df %>% 
    na.omit()%>%
    gather(dv, score, -sub_id) %>%
    mutate(time = ifelse(grepl('\\.x', dv), 1, ifelse(grepl('\\.y', dv), 2, NA))) %>%
    separate(dv, c("dv", "drop"), sep='\\.([^.]*)$') %>%
    select(-drop)
  
  
  if(format == 'wide'){
    df = df%>% spread(time, score) 
  }
  
  return(df)
}

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
  
  rho = cor(df$`1`, df$`2`, method='pearson')
  
  return(rho)
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