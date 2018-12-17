# Load data

library(tidyverse)
library(jsonlite)

fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_bw())
ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_manual(values=cbbPalette) + scale_color_manual(values=cbbPalette)+theme(legend.position="bottom")

file_names = c('SRO_Retest_Analyses_Helper_Functions.R', 'battery_completion_data.R', 'lit_review_data.R', 'subject_data.R', 'intratrial_rel_data.R', 'demog_data.R', 'boot_rel_data.R', 'rel_comp_data.R', 'duplicate_items_data.R', 'measure_labels_data.R', 'meaningful_vars_data.R', 'bayesian_models_data.R')

if(from_gh){
  library(RCurl)
  
  workspace_scripts = 'https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/workspace_scripts/'
  
  test_data_path = 'https://raw.githubusercontent.com/zenkavi/Self_Regulation_Ontology/master/Data/Complete_03-29-2018/'
  
  retest_data_path = 'https://raw.githubusercontent.com/zenkavi/Self_Regulation_Ontology/retest_scripts/Data/Retest_03-29-2018/'
  
  for(file_name in file_names){
    eval(parse(text = getURL(paste0(workspace_scripts,file_name), ssl.verifypeer = FALSE)))
  }
} else{
  workspace_scripts = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/'
  
  test_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Complete_03-29-2018/'
  
  retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'
  
  for(file_name in file_names){
    source(paste0(workspace_scripts,file_name))
  }
}


