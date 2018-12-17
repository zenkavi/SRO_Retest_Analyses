library(gridExtra)
library(lme4)
library(GGally)
library(rmarkdown)
library(stringr)
library(plotly)
library(DT)
library(RecordLinkage)
# library(MCMCglmm)

render_this <- function(){rmarkdown::render('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/SRO_Retest_Analyses.Rmd', output_dir = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/reports', html_notebook(toc = T, toc_float = T, toc_depth = 2, code_folding = 'hide'))}

options(scipen = 1, digits = 4)

file_names = c('g_legend.R', 'g_caption.R', 'sem.R', 'trim.R', 'get_numeric_cols.R', 'match_t1_t2.R', 'get_retest_stats.R', 'process_boot_df.R', 'make_rel_df.R')

if(from_gh){
  require(RCurl)
  helper_func_path = 'https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/'
  for(file_name in file_names){
    eval(parse(text = getURL(paste0(helper_func_path,file_name), ssl.verifypeer = FALSE)))
  }
  
} else{
  helper_func_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/' 
  for(file_name in file_names){
    source(paste0(helper_func_path,file_name))
  }
}



