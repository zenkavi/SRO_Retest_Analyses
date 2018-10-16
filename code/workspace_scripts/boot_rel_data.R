#########################
## Bootstrapped reliability data ####
#########################

### Demographics

demog_boot_df <- read.csv(gzfile(paste0(retest_data_path,'demog_boot_merged.csv.gz')))

demog_boot_df = process_boot_df(demog_boot_df)

### Task and survey measures

boot_df <- read.csv(gzfile(paste0(retest_data_path,'bootstrap_merged.csv.gz')))

boot_df = process_boot_df(boot_df)

boot_df = boot_df[boot_df$dv %in% retest_report_vars,]

# Check if you have all variables bootstrapped
# retest_report_vars[which(retest_report_vars %in% boot_df$dv==FALSE)]

# Boot df contains hddm parameters fit on the full sample in the t1 data
# refits_bootstrap_merged.csv.gz contains bootstrapped reliabilities

refit_boot_df = read.csv(gzfile(paste0(retest_data_path,'refits_bootstrap_merged.csv.gz')))

refit_boot_df = process_boot_df(refit_boot_df)

fullfit_boot_df = boot_df[as.character(boot_df$dv) %in% unique(as.character(refit_boot_df$dv)),]

boot_df = boot_df[!as.character(boot_df$dv) %in% unique(as.character(refit_boot_df$dv)),]

boot_df = rbind(boot_df, refit_boot_df)

rm(refit_boot_df)

boot_df = boot_df %>% mutate(var_subs_pct = var_subs/(var_subs+var_ind+var_resid)*100,
                             var_ind_pct = var_ind/(var_subs+var_ind+var_resid)*100,
                             var_resid_pct = var_resid/(var_subs+var_ind+var_resid)*100)
# 
# var_boot_df = boot_df %>%
#   group_by(dv) %>%
#   summarise(mean_icc = mean(icc),
#             mean_pearson = mean(pearson))
# 
# rel_comp = lit_review %>%
#   left_join(var_boot_df, by = 'dv')
# 
# write.csv(rel_comp, '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/rel_comp.csv', row.names=F)
# 
# rm(var_boot_df)