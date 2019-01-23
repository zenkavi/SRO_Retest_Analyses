source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('pos_log') | !exists('neg_log')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/transform_remove_skew.R')
}

if(!exists('rel_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/subject_data.R')
  
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/make_rel_df.R')
  
  rel_df = make_rel_df(t1_df = test_data, t2_df = retest_data, metrics = c('spearman', 'icc2.1', 'pearson', 'var_breakdown', 'partial_eta', 'sem'))
  
  rel_df$task = 'task'
  rel_df[grep('survey', rel_df$dv), 'task'] = 'survey'
  rel_df[grep('holt', rel_df$dv), 'task'] = "task"
  rel_df = rel_df %>%
    select(dv, task, spearman, icc2.1, pearson, partial_eta, sem, var_subs, var_ind, var_resid)
}

if(!exists('retest_data_path')){
  retest_data_path = "/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_12-19-2018/"
}
# List transformed t1 variables

tmp = read.csv(paste0(retest_data_path, 'meaningful_variables_clean.csv'))

poslog_vars = grep(".logTr", names(tmp), value = TRUE)
poslog_vars = gsub(".logTr", "", poslog_vars)
poslog_vars = poslog_vars[poslog_vars %in% names(test_data)]
neglog_vars = grep(".ReflogTr", names(tmp), value = TRUE)
neglog_vars = gsub(".ReflogTr", "", neglog_vars)
neglog_vars = neglog_vars[neglog_vars %in% names(test_data)]

# Transform these variables in the t1 data

test_data_tr = test_data[,c("sub_id", poslog_vars, neglog_vars)]

for(i in names(test_data_tr)){
  if(i %in% poslog_vars){
    test_data_tr[,i] = pos_log(test_data_tr[,i])
  }
  if(i %in% neglog_vars){
    test_data_tr[,i] = neg_log(test_data_tr[,i])
  }
}

# Transform the vars from t1 in retest data

retest_data_tr = retest_data[,c("sub_id", poslog_vars, neglog_vars)]

for(i in names(retest_data_tr)){
  if(i %in% poslog_vars){
    retest_data_tr[,i] = pos_log(retest_data_tr[,i])
  }
  if(i %in% neglog_vars){
    retest_data_tr[,i] = neg_log(retest_data_tr[,i])
  }
}

# Calculate point estimates of reliability for transformed vars

rel_df_tr = make_rel_df(test_data_tr, retest_data_tr, metrics=c('icc2.1', "pearson", "spearman"))

# Correlate transformed var ICC's with non-transformed vars' ICCs

p = rel_df %>%
  select(dv, icc2.1, pearson, spearman) %>%
  filter(dv %in% rel_df_tr$dv) %>%
  gather(key, value, -dv) %>%
  left_join(rel_df_tr %>% 
              gather(key, value, -dv), by=c("dv", "key")) %>%
  ggplot(aes(value.x, value.y))+
  geom_point()+
  geom_abline(aes(slope=1, intercept=0))+
  facet_wrap(~key)+
  xlab("Reliability of non-transformed DV")+
  ylab("Reliability of transformed DV")+
  theme(aspect.ratio = 1)

ggsave(paste0('Transformation_Effects.', out_device), plot=p, device = out_device, path = fig_path, width = 9, height = 3, units = "in", limitsize = FALSE)
