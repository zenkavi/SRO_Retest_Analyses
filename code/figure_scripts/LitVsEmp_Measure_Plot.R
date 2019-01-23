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

p1 = rel_comp %>%
  filter(task == 'task') %>%
  ggplot(aes(x = var, y = retest_reliability)) +
  geom_boxplot(fill='#00BFC4', position="identity")+
  geom_point(aes(x = var, y = mean_pearson),color="purple", fill="purple", size=3, shape=23)+
  coord_flip()+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80"),
        legend.position = 'bottom') +
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))

p2 = rel_comp %>%
  filter(task == 'survey') %>%
  ggplot(aes(x = var, y = retest_reliability)) +
  geom_boxplot(fill='#F8766D', position="identity")+
  geom_point(aes(x = var, y = mean_pearson),color="purple", fill="purple", size=3, shape=23)+
  coord_flip()+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80"),
        legend.position = 'bottom') +
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))

p3 <- arrangeGrob(p1, p2,nrow=1)

ggsave(paste0('LitVsEmp_Measure_Plot.',out_device),plot=p3, device = out_device, path = fig_path, width = 24, height = 20, units = "in", dpi=img_dpi)

rm(p1, p2, p3)