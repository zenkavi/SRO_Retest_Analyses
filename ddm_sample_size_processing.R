#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#Usage:
#Rscript --vanilla ddm_sample_size_processing.R data_dir out_dir

#test if all arguments are supplied
# test if there is at least one argument: if not, return an error
if (length(args)<2) {
  stop("Arguments are missing. Usage: Rscript --vanilla ddm_sample_size_processing.R data_dir out_dir", call.=FALSE)
} 

data_dir <- args[1] 
output_dir <- args[2]

#load packages
library(dplyr)
library(tidyr)
library(psych)

all_ddm_sample_size = read.csv(paste0(data_dir, 'all_ddm_sample_size_estimates.csv'))

all_ddm_sample_size = all_ddm_sample_size %>%
  drop_na()

all_ddm_sample_size_summary = all_ddm_sample_size %>%
  mutate(icc = as.numeric(as.character(icc)),
         spearman = as.numeric(as.character(spearman)),
         eta_sq = as.numeric(as.character(eta_sq)),
         sem = as.numeric(as.character(sem))) %>%
  group_by(dv, N, rep) %>%
  summarise(icc_median = quantile(icc, probs = 0.5),
            icc_2.5 = quantile(icc, probs = 0.025),
            icc_97.5 = quantile(icc, probs = 0.975),
            spearman_median = quantile(spearman, probs = 0.5),
            spearman_2.5 = quantile(spearman, probs = 0.025),
            spearman_97.5 = quantile(spearman, probs = 0.975),
            eta_median = quantile(spearman, probs = 0.5),
            eta_2.5 = quantile(spearman, probs = 0.025),
            eta_97.5 = quantile(spearman, probs = 0.975),
            sem_median = quantile(spearman, probs = 0.5),
            sem_2.5 = quantile(spearman, probs = 0.025),
            sem_97.5 = quantile(spearman, probs = 0.975))

write.csv(all_ddm_sample_size_summary, paste0(output_dir, 'all_ddm_sample_size_summary.csv'), row.names=FALSE)