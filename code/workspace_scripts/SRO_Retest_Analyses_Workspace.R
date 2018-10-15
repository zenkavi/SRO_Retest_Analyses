# Load data

library(tidyverse)
library(jsonlite)

workspace_scripts = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/'

source(paste0(workspace_scripts,'SRO_Retest_Analyses_Helper_Functions.R'))

test_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Complete_03-29-2018/'

retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'

fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

source(paste0(workspace_scripts, 'battery_completion_data.R'))

source(paste0(workspace_scripts,'lit_review_data.R'))

source(paste0(workspace_scripts,'subject_data.R'))

source(paste0(workspace_scripts,'intratrial_rel_data.R'))

source(paste0(workspace_scripts,'demog_data.R'))

source(paste0(workspace_scripts,'boot_rel_data.R'))

source(paste0(workspace_scripts,'duplicate_items_data.R'))

source(paste0(workspace_scripts,'measure_labels_data.R'))

source(paste0(workspace_scripts,'meaningful_vars_data.R'))

source(paste0(workspace_scripts,'bayesian_models_data.R'))
