fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

if(!exists('from_gh')){
  from_gh=FALSE
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('trial_num_rel_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/intratrial_rel_data.R')
}

trial_num_rel_df %>%
  gather(key, value, -breaks) %>%
  filter(key %in% c("avg_rt_icc", "learning_to_learn_icc", "nonperseverative_errors_icc")) %>%
  ggplot(aes((breaks+1)*10, value, shape=key))+
  geom_point()+
  geom_line(alpha = 0.5)+
  theme_bw()+
  xlab("Number of trials")+
  ylab("ICC")+
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12)) +
  scale_shape_manual(values = c(0:9),
                     breaks = c("avg_rt_icc","learning_to_learn_icc", "nonperseverative_errors_icc"),
                     labels = c("Average Response Time", "Learning to Learn",  "Nonperseverative Errors"))

ggsave(paste0('Shift_Task_IntratrialRel.', out_device), device = out_device, path = fig_path, width = 7, height = 5.4, units = "in", limitsize = FALSE, dpi = img_dpi)
