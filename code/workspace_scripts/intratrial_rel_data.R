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

#########################
## Intratrial reliability data ####
#########################

# t1_dvs = read.csv(paste0(retest_data_path, 't1_shift_dvs.csv'))
# t2_dvs = read.csv(paste0(retest_data_path, 't2_shift_dvs.csv'))

# t1_dvs = t1_dvs %>% select(-X)
# t2_dvs = t2_dvs %>% select(-X)

# hr_merge = merge(t1_dvs, t2_dvs, by = c("worker_id", "level_1"))
# 
# hr_merge = hr_merge %>%
#   gather(key, value, -worker_id, -level_1) %>%
#   separate(key, c("dv", "time"), sep="\\.") %>%
#   mutate(time = ifelse(time == "x", 1, 2))%>%
#   rename(sub_id = worker_id,
#          breaks = level_1)
# 
# t1_dvs = hr_merge %>%
#   filter(time == 1) %>%
#   select(-time) %>%
#   spread(dv, value)
# 
# t2_dvs = hr_merge %>%
#   filter(time == 2) %>%
#   select(-time) %>%
#   spread(dv, value)

# calculate point estimates for reliability of each of the variables for each break
# get_retest_stat for each break of tmp_t1_dvs and tmp_t2_dvs

# trial_num_rel_df = data.frame(breaks=rep(NA, length(unique(t1_dvs$breaks))),
#                               acc_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               avg_rt_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               conceptual_responses_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               fail_to_maintain_set_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               learning_rate_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               learning_to_learn_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               nonperseverative_errors_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               perseverative_errors_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               perseverative_responses_icc=rep(NA, length(unique(t1_dvs$breaks))),
#                               total_errors_icc=rep(NA, length(unique(t1_dvs$breaks))))
# 
# for(i in 1:length(unique(t1_dvs$breaks))){
#   cur_break = unique(t1_dvs$breaks)[i]
#   tmp_t1_dvs = t1_dvs %>% filter(breaks == cur_break)
#   tmp_t2_dvs = t2_dvs %>% filter(breaks == cur_break)
#   trial_num_rel_df$breaks[i] = cur_break
#   trial_num_rel_df$acc_icc[i] = get_retest_stat("acc", tmp_t1_dvs, tmp_t2_dvs)
#   trial_num_rel_df$avg_rt_icc[i] = get_retest_stat("avg_rt", tmp_t1_dvs, tmp_t2_dvs)
#   trial_num_rel_df$conceptual_responses_icc[i] = get_retest_stat("conceptual_responses", tmp_t1_dvs, tmp_t2_dvs, metric=c('icc'))
#   trial_num_rel_df$fail_to_maintain_set_icc[i] = get_retest_stat("fail_to_maintain_set", tmp_t1_dvs, tmp_t2_dvs, metric=c('icc'))
#   trial_num_rel_df$learning_rate_icc[i] = get_retest_stat("learning_rate", tmp_t1_dvs, tmp_t2_dvs, metric=c('icc'))
#   trial_num_rel_df$learning_to_learn_icc[i] = get_retest_stat("learning_to_learn", tmp_t1_dvs, tmp_t2_dvs, metric=c('icc'))
#   trial_num_rel_df$nonperseverative_errors_icc[i] = get_retest_stat("nonperseverative_errors", tmp_t1_dvs, tmp_t2_dvs, metric=c('icc'))
#   trial_num_rel_df$perseverative_errors_icc[i] = get_retest_stat("perseverative_errors", tmp_t1_dvs, tmp_t2_dvs, metric=c('icc'))
#   trial_num_rel_df$perseverative_responses_icc[i] = get_retest_stat("perseverative_responses", tmp_t1_dvs, tmp_t2_dvs, metric=c('icc'))
#   trial_num_rel_df$total_errors_icc[i] = get_retest_stat("total_errors", tmp_t1_dvs, tmp_t2_dvs, metric=c('icc'))
# }
# rm(i, cur_break, tmp_t1_dvs, tmp_t2_dvs)
# 
# trial_num_rel_df$breaks = as.numeric(trial_num_rel_df$breaks)

# write.csv(trial_num_rel_df, paste0(retest_data_path, 'trial_num_rel_df_shift.csv'))

trial_num_rel_df = read.csv(paste0(retest_data_path, 'trial_num_rel_df_shift.csv'))