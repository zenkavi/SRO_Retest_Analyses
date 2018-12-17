
#########################
## Bayesian models ####
#########################
# tmp = boot_df %>%
#   select(dv, task, var_subs_pct, var_ind_pct, var_resid_pct)

#summary(lmerTest::lmer(icc ~  task + (1|dv), boot_df))
# m = MCMCglmm(icc ~  task, random = ~dv, data=boot_df, nitt = 1300, burnin = 300)
icc_by_task_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/icc_by_task_model.rds')


# summary(lmerTest::lmer(var_subs_pct~task+(1|dv),tmp%>%select(-var_ind_pct,-var_resid_pct)))
var_subs_pct_by_task_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/var_subs_pct_by_task_model.rds')

# summary(lmerTest::lmer(var_ind_pct~task+(1|dv),tmp%>%select(-var_subs_pct,-var_resid_pct)))
var_ind_pct_by_task_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/var_ind_pct_by_task_model.rds')

# summary(lmerTest::lmer(var_resid_pct~task+(1|dv),tmp%>%select(-var_subs_pct,-var_ind_pct)))
var_resid_pct_by_task_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/var_resid_pct_by_task_model.rds')

# tmp = measure_labels %>%
#   mutate(dv = as.character(dv)) %>%
#   filter(task == 'task',
#          dv %in% meaningful_vars) %>%
#   left_join(boot_df[,c("dv", "icc")], by = 'dv') %>%
#   separate(dv, c('task_name', 'extra_1', 'extra_2'), sep = '\\.',remove=FALSE) %>%
#   select(-extra_1, -extra_2)

# summary(lm(icc ~ num_all_trials, data = tmp))
# summary(lmerTest::lmer(icc ~ num_all_trials + (1|dv), data = tmp))
icc_by_num_trials_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/icc_by_num_trials_model.rds')

# tmp = measure_labels %>%
#   mutate(dv = as.character(dv),
#          contrast = ifelse(overall_difference == "difference", "contrast", "non-contrast")) %>%
#   filter(ddm_task == 1,
#          rt_acc != 'other') %>%
#   drop_na() %>%
#   left_join(boot_df[,c("dv", "icc")], by = 'dv')

# summary(lmerTest::lmer(icc ~ raw_fit + (1|dv) ,tmp %>% filter(contrast == "non-contrast")))
icc_by_rawfit_noncon_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/icc_by_rawfit_noncon_model.rds')

# summary(lmerTest::lmer(icc ~ raw_fit + (1|dv) ,tmp %>% filter(contrast == "contrast")))
icc_by_rawfit_con_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/icc_by_rawfit_con_model.rds')
