if(from_gh){
  require(RCurl)
}

if(!exists('test_data_path')){
  if(from_gh){
    test_data_path = 'https://raw.githubusercontent.com/zenkavi/Self_Regulation_Ontology/master/Data/Complete_03-29-2018/'
  } else{
    test_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Complete_03-29-2018/' 
  }
}

if(!exists('retest_data_path')){
  if(from_gh){
    retest_data_path = 'https://raw.githubusercontent.com/zenkavi/Self_Regulation_Ontology/retest_scripts/Data/Retest_03-29-2018/'
  } else{
    retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'
  }
}

if(!exists('process_boot_df')){
  if(from_gh){
    eval(parse(text = getURL('https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/process_boot_df.R', ssl.verifypeer = FALSE)))
  }
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/process_boot_df.R')
}

if(!exists('grabRemoteGz')){
  if(from_gh){
    eval(parse(text = getURL('https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/grabRemoteGz.R', ssl.verifypeer = FALSE)))
  }
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/grabRemoteGz.R')
}

if(!exists('retest_report_vars')){
  tmp1 <- read.csv(paste0(test_data_path,'meaningful_variables.csv'))
  tmp2 <- read.csv(paste0(test_data_path,'meaningful_variables_noDDM.csv'))
  tmp3 <- read.csv(paste0(test_data_path,'meaningful_variables_EZ.csv'))
  retest_report_vars = c(names(tmp1), names(tmp2), names(tmp3))
  retest_report_vars = unique(retest_report_vars)
  if(!exists('lit_review')){
    source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/lit_review_data.R')
  }
  lit_rev_vars = as.character(unique(lit_review$dv)[which(unique(lit_review$dv) %in% retest_report_vars == FALSE)])
  retest_report_vars = c(retest_report_vars, lit_rev_vars)
  rm(tmp1, tmp2, tmp3, lit_rev_vars)
}

#########################
## Bootstrapped reliability data ####
#########################

### Demographics

if(from_gh){
  demog_boot_df = grabRemoteGz(retest_data_path, 'demog_boot_merged.csv.gz')
} else{
  demog_boot_df <- read.csv(gzfile(paste0(retest_data_path,'demog_boot_merged.csv.gz')))
}

demog_boot_df = process_boot_df(demog_boot_df)

### Task and survey measures

if(from_gh){
  boot_df = grabRemoteGz(retest_data_path, 'bootstrap_merged.csv.gz')
} else{
  boot_df <- read.csv(gzfile(paste0(retest_data_path,'bootstrap_merged.csv.gz')))
}

boot_df = process_boot_df(boot_df)

boot_df = boot_df[boot_df$dv %in% retest_report_vars,]

# Check if you have all variables bootstrapped
# retest_report_vars[which(retest_report_vars %in% boot_df$dv==FALSE)]

# Boot df contains hddm parameters fit on the full sample in the t1 data
# refits_bootstrap_merged.csv.gz contains bootstrapped reliabilities

if(from_gh){
  refit_boot_df = grabRemoteGz(retest_data_path, 'refits_bootstrap_merged.csv.gz')
} else{
  refit_boot_df <- read.csv(gzfile(paste0(retest_data_path,'refits_bootstrap_merged.csv.gz')))
}

refit_boot_df = process_boot_df(refit_boot_df)

fullfit_boot_df = boot_df[as.character(boot_df$dv) %in% unique(as.character(refit_boot_df$dv)),]

boot_df = boot_df[!as.character(boot_df$dv) %in% unique(as.character(refit_boot_df$dv)),]

boot_df = rbind(boot_df, refit_boot_df)

rm(refit_boot_df)

boot_df = boot_df %>% mutate(var_subs_pct = var_subs/(var_subs+var_ind+var_resid)*100,
                             var_ind_pct = var_ind/(var_subs+var_ind+var_resid)*100,
                             var_resid_pct = var_resid/(var_subs+var_ind+var_resid)*100)

# var_boot_df = boot_df %>%
#   group_by(dv) %>%
#   summarise(mean_icc2.1 = mean(icc2.1),
#             mean_icc3.k = mean(icc3.k),
#             mean_pearson = mean(pearson))
# 
# rel_comp = lit_review %>%
#   left_join(var_boot_df, by = 'dv')
# 
# write.csv(rel_comp, '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/rel_comp.csv', row.names=F)
# 
# rm(var_boot_df)
