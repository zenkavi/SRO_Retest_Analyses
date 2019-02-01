fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

if(!exists('from_gh')){
  from_gh=FALSE
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('retest_data') | !exists('test_data')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/subject_data.R')
}

if(!exists('task_completion_times')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/battery_completion_data.R')
}

if(!exists('get_numeric_cols')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/get_numeric_cols.R')
}

if(!exists('match_t1_t2')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/match_t1_t2.R')
}

numeric_cols = get_numeric_cols(test_data)

t1_2_difference = data.frame()

for(i in 1:length(numeric_cols)){
  tmp = match_t1_t2(numeric_cols[i],t1_df=test_data ,t2_df=retest_data,format='wide')
  tmp = tmp %>%
    mutate(difference = scale(`2` - `1`))
  t1_2_difference = rbind(t1_2_difference, tmp)
}

t1_2_difference$difference = as.data.frame(t1_2_difference$difference)$V1

t1_2_difference = t1_2_difference %>% separate(dv, c("task", "dv2"), sep="\\.", remove=FALSE)

rm(tmp, i, numeric_cols)

t1_2_difference = merge(t1_2_difference, task_comp_times[,c('sub_id', 'task','days_btw')], by=c('sub_id', 'task'))

rm(task_comp_times)

t1_2_difference %>%
  ggplot()+
  geom_smooth(aes(as.numeric(days_btw), abs(difference), group=factor(dv)), method='lm', se=FALSE, color = 'grey', alpha = 0.5)+
  geom_smooth(aes(as.numeric(days_btw), abs(difference)), method='lm', color = "black", se=FALSE)+
  theme(legend.title = element_text(size=10),
        axis.text = element_text(size=10))+
  xlab('Days between completion')+
  ylab('Scaled difference score')

ggsave(paste0('Days_Btw_Completion.', out_device), device = out_device, path = fig_path, width = 3.4, height = 3, units = "in", limitsize = FALSE, dpi = img_dpi)
