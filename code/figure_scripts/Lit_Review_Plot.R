source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('lit_review')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/lit_review_data.R')
}

if(!exists('g_legend')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/g_legend.R')
}
p1_legend = lit_review %>%
  filter(task == 'task') %>%
  ggplot(aes(y = var, x = retest_reliability)) +
  geom_point(aes(size=sample_size, shape=type))+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80"),
        legend.position = 'bottom') +
  xlab("")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_shape_manual(breaks = sort(lit_review$type), values = c(15, 16, 17, 3))

p1 = lit_review %>%
  filter(task == 'task') %>%
  ggplot(aes(y = var, x = retest_reliability)) +
  geom_point(aes(size=sample_size, shape = type), color='#00BFC4')+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80"),
        legend.position = 'bottom') +
  xlab("")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_shape_manual(breaks = sort(lit_review$type), values = c(15, 16, 17, 3))

p2 = lit_review %>%
  filter(task == 'survey') %>%
  ggplot(aes(y = var, x = retest_reliability)) +
  geom_point(aes(size=sample_size, shape = type), color = '#F8766D')+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80")) +
  xlab("")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_shape_manual(breaks = sort(lit_review$type), values = c(15, 17, 3))

mylegend<-g_legend(p1_legend)

p3 <- arrangeGrob(arrangeGrob(p1 +theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 1))

ggsave(paste0('Lit_Review_Plot_t.',out_device), plot = p3, device = out_device, path = fig_path, width = 24, height = 20, units = "in", dpi=img_dpi)

rm(p1, p2, p3, p1_legend, mylegend)
