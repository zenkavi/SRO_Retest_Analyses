if(!exists('fig_path')){
  fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'
}

if(!exists('from_gh')){
  from_gh=FALSE
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('rel_comp')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/rel_comp_data.R')
}

rel_comp %>%
  mutate(task = factor(task, levels = c("task", "survey"), labels = c("Task", "Survey"))) %>%
  group_by(dv, task) %>%
  summarise(mean_lit = mean(retest_reliability),
            mean_emp = unique(mean_pearson)) %>%
  ggplot(aes(mean_emp, mean_lit, col=task, shape=task))+
  geom_smooth(method="lm")+
  geom_point()+
  xlim(-0.25, 1)+
  ylim(-0.25, 1)+
  ylab("Average Literature\nReliability Estimate")+
  xlab("Average Empirical Reliability Estimate")+
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.key.size = unit(0.25,"cm"))

ggsave(paste0('LitAndEmpAveCorr_Plot.', out_device), device = out_device, path = fig_path, width = 3.4, height = 2, units = "in", dpi = img_dpi)
