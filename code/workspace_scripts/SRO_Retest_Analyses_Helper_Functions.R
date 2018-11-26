library(gridExtra)
library(lme4)
library(GGally)
library(rmarkdown)
library(stringr)
library(plotly)
library(DT)
library(RecordLinkage)
library(MCMCglmm)
library(RCurl)

theme_set(theme_bw())

render_this <- function(){rmarkdown::render('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/SRO_Retest_Analyses.Rmd', output_dir = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/reports', html_notebook(toc = T, toc_float = T, toc_depth = 2, code_folding = 'hide'))}

options(scipen = 1, digits = 4)

helper_func_path = 'https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/'

eval(parse(text = getURL(paste0(helper_func_path,'g_legend.R'), ssl.verifypeer = FALSE)))
eval(parse(text = getURL(paste0(helper_func_path,'g_caption.R'), ssl.verifypeer = FALSE)))
eval(parse(text = getURL(paste0(helper_func_path,'sem.R'), ssl.verifypeer = FALSE)))
eval(parse(text = getURL(paste0(helper_func_path,'trim.R'), ssl.verifypeer = FALSE)))
eval(parse(text = getURL(paste0(helper_func_path,'get_numeric_cols.R'), ssl.verifypeer = FALSE)))
eval(parse(text = getURL(paste0(helper_func_path,'match_t1_t2.R'), ssl.verifypeer = FALSE)))
eval(parse(text = getURL(paste0(helper_func_path,'get_retest_stats.R'), ssl.verifypeer = FALSE)))
eval(parse(text = getURL(paste0(helper_func_path,'process_boot_df.R'), ssl.verifypeer = FALSE)))
eval(parse(text = getURL(paste0(helper_func_path,'make_rel_df.R'), ssl.verifypeer = FALSE)))
