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

theme_set(theme_bw())

render_this <- function(){rmarkdown::render('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/SRO_Retest_Analyses.Rmd', output_dir = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/reports', html_notebook(toc = T, toc_float = T, toc_depth = 2, code_folding = 'hide'))}

options(scipen = 1, digits = 4)

helper_func_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/SRO_Retest_Helper_Functions/'

source(paste0(helper_func_path, 'g_legend.R'))
source(paste0(helper_func_path, 'sem.R'))
source(paste0(helper_func_path, 'trim.R'))
source(paste0(helper_func_path, 'match_t1_t2.R'))
source(paste0(helper_func_path, 'get_retest_stats.R'))
source(paste0(helper_func_path, 'process_boot_df.R'))