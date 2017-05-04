#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#Usage:
#Rscript --vanilla ddm_sample_size_.R data_dir out_dir rep_time sample_size dv_name

#test if all arguments are supplied
# test if there is at least one argument: if not, return an error
if (length(args)<3) {
  stop("Arguments are missing. Usage: Rscript --vanilla ddm_sample_size.R data_dir out_dir rep_time sample_size dv_name", call.=FALSE)
} 

data_dir <- args[1] 
output_dir <- args[2]
rep_times <- as.numeric(args[3])
sample_size <- as.numeric(args[4])
dv_name <- args[5]

#load packages
library(dplyr)
library(tidyr)
library(psych)

raw_pre_correction <- read.csv(paste0(data_dir,'raw_retest_subs_test_data_preddm_correction.csv'))

raw_retest_data <- read.csv(paste0(data_dir,'raw_retest_data.csv'))

match_t1_t2 <- function(dv_var, t1_df = retest_subs_test_data, t2_df = retest_data, merge_var = 'sub_id', format = "long", sample = 'full', sample_vec){
  
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

get_spearman = function(dv_var, t1_df = retest_subs_test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
  
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide')
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, format='wide', sample='bootstrap', sample_vec = sample_vec)
  }
  
  rho = cor(df$`1`, df$`2`, method='spearman')
  
  return(rho)
}

get_icc <- function(dv_var, t1_df = retest_subs_test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
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

get_eta <- function(dv_var, t1_df = retest_subs_test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
  if(sample=='full'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var)
  }
  else if(sample=='bootstrap'){
    df = match_t1_t2(dv_var, t1_df = t1_df, t2_df = t2_df, merge_var = merge_var, sample='bootstrap', sample_vec = sample_vec)
  }
  
  mod = summary(aov(score~Error(sub_id)+time, df))
  ss_time = as.data.frame(unlist(mod$`Error: Within`))['Sum Sq1',]
  ss_error = as.data.frame(unlist(mod$`Error: Within`))['Sum Sq2',]
  eta = ss_time/(ss_time+ss_error)
  return(eta)
}

get_sem <- function(dv_var, t1_df = retest_subs_test_data, t2_df = retest_data, merge_var = 'sub_id', sample='full', sample_vec){
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

sample_workers = function(N = 150, repl= TRUE, df=retest_data, worker_col = "sub_id"){
  return(sample(df[,worker_col], N, replace = repl))
}

bootstrap_relialibility = function(metric = c('icc', 'spearman', 'eta_sq', 'sem'), dv_var, N=150, t1_df, t2_df){
  tmp_sample = sample_workers(N, df = t1_df)
  out_df = data.frame(dv = dv_var)
  if('icc' %in% metric){
    out_df$icc = get_icc(dv_var, sample = 'bootstrap', sample_vec = tmp_sample, t1_df = t1_df, t2_df = t2_df)
  }
  if('spearman' %in% metric){
    out_df$spearman = get_spearman(dv_var, sample = 'bootstrap', sample_vec = tmp_sample, t1_df = t1_df, t2_df = t2_df)
  }
  if('eta_sq' %in% metric){
    out_df$eta_sq = get_eta(dv_var, sample = 'bootstrap', sample_vec = tmp_sample, t1_df = t1_df, t2_df = t2_df)
  }
  if('sem' %in% metric){
    out_df$sem = get_sem(dv_var, sample = 'bootstrap', sample_vec = tmp_sample, t1_df = t1_df, t2_df = t2_df)
  }
  return(out_df)
}

output_df = plyr::rdply(rep_times, bootstrap_relialibility(dv_var = dv_name, t1_df = raw_pre_correction, t2_df = raw_retest_data, N=sample_size))

output_df = output_df %>%
  mutate(N = sample_size,
         rep = rep_times)

write.csv(output_df, paste0(output_dir, dv_name, '_rep', rep_times, '_sampleN', sample_size, '_output.csv'))

