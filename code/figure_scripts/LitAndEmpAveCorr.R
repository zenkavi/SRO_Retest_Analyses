fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

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
  geom_smooth(method="lm", se=FALSE)+
  geom_point(alpha = 0.3)+
  xlim(-0.25, 1)+
  ylim(-0.25, 1)+
  ylab("Reliability in Literature")+
  xlab("Reliability in New Data")+
  theme(legend.title = element_blank(), 
        legend.position = c(.85, .15),
        legend.key.size = unit(0.25,"cm"),
        legend.text = element_text(size=8),
        axis.text = element_text(size=8),
        axis.title = element_text(size=8),
        plot.margin = margin(.25,.25,.25,.25, "cm"),
        panel.grid = element_blank(),
        aspect.ratio = 0.7)+
  geom_abline(aes(slope=1, intercept=0), linetype="dashed")

ggsave(paste0('LitAndEmpAveCorr_Plot.', out_device), device = out_device, path = fig_path, width = 3.4, height = 2, units = "in", dpi = img_dpi)
