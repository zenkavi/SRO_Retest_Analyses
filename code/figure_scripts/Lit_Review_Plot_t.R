if(!exists('fig_path')){
  fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'
}

if(!exists('from_gh')){
  from_gh=FALSE
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('lit_review')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/lit_review_data.R')
}

if(!exists('g_legend')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/g_legend.R')
}

if(!exists('g_caption')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/g_caption.R')
}

p1_t <- lit_review %>%
  filter(task == 'task') %>%
  ggplot(aes(y = factor(task_group, levels=rev(unique(task_group))), x = retest_reliability)) +
  geom_point(color="#E69F00", size=4)+
  theme(axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=9)) +
  xlab("Retest Reliability")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_vline(xintercept = 0, color = "red", size = 1)

p2_t <- lit_review %>%
  filter(task == 'survey') %>%
  ggplot(aes(y = factor(task_group, levels=rev(unique(task_group))), x = retest_reliability)) +
  geom_point(color="#56B4E9", size=4)+
  theme(axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=9)) +
  xlab("Retest Reliability")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_vline(xintercept = 0, color = "red", size = 1)

p3_t <- arrangeGrob(p1_t, p2_t, nrow=1)

ggsave(paste0('Lit_Review_Plot_t.',out_device), plot = p3_t, device = out_device, path = fig_path, width = 7, height = 10.5, units = "in", dpi=img_dpi)

# rm(p1_t, p2_t, p3_t, mylegend, p1_t_legend)
