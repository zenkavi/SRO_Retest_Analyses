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

p1_t_legend <- lit_review %>%
  filter(task == 'task') %>%
  ggplot(aes(y = factor(task_group, levels=rev(unique(task_group))), x = retest_reliability)) +
  geom_point(aes(size=sample_size, shape = type), color='black')+
  theme(axis.text.y = element_text(size=30),
        legend.position = 'bottom',
        axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        legend.text = element_text(size=16),
        legend.key.width = unit(0.75, "inches"),
        legend.title = element_text(size=20),
        legend.spacing.x = unit(0.5, "inches"),
        legend.box = "vertical",
        legend.direction = "horizontal",
        plot.caption = element_text(family = "Times", size = 40, lineheight = 2, hjust = 0)) +
  xlab("Retest Reliability")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_shape_manual(breaks = sort(unique(lit_review$type)), values = c(17, 3, 16, 15), name="Type")+
  scale_size_continuous(name = "Sample Size",
                        limits = c(10, 1200),
                        range = c(1, 40),
                        breaks = c(10, 100, 1000))+
  guides(#size = guide_legend(override.aes = list(breaks = c(10, 100, 1000)), labels = c(10, 100, 1000)),
    shape = guide_legend(override.aes = list(size = 16)))+
  geom_vline(xintercept = 0, color = "red", size = 1)+
  labs(caption = str_wrap("FIGURE 1: Summary of the literature review for tasks (left) and surveys (right). Each point represents a study containing test-retest reliability data on an unspecified dependent measure for a given task. The size of the point depends on the sample size of the study and the shape depends on the metric that was used to estimate reliability. Each vertical red line indicates 0 reliability.", width = 120))

p1_t <- lit_review %>%
  filter(task == 'task') %>%
  ggplot(aes(y = factor(task_group, levels=rev(unique(task_group))), x = retest_reliability)) +
  geom_point(aes(size=sample_size, shape = type), color='#00BFC4')+
  theme(axis.text.y = element_text(size=30),
        legend.position = 'none',
        axis.text.x = element_text(size=23),
        axis.title.x = element_text(size=30)) +
  xlab("Retest Reliability")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_shape_manual(breaks = sort(unique(lit_review$type)), values = c(17, 3, 16, 15))+
  scale_size_continuous(range = c(1, 40),
                        limits = c(10,1200))+
  geom_vline(xintercept = 0, color = "red", size = 1)

p2_t <- lit_review %>%
  filter(task == 'survey') %>%
  ggplot(aes(y = factor(task_group, levels=rev(unique(task_group))), x = retest_reliability)) +
  geom_point(aes(size=sample_size, shape = type), color='#F8766D')+
  theme(axis.text.y = element_text(size=30),
        legend.position = 'none',
        axis.text.x = element_text(size=23),
        axis.title.x = element_text(size=30)) +
  xlab("Retest Reliability")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_shape_manual(breaks = c("ICC","Pearson","Spearman"),
                     values = c(17, 16, 15))+
  scale_size_continuous(range = c(1, 40),
                        limits = c(10,1200))+
  geom_vline(xintercept = 0, color = "red", size = 1)

mylegend<-g_legend(p1_t_legend)
mycaption<-g_caption(p1_t_legend)

p3_t <- arrangeGrob(arrangeGrob(p1_t, p2_t, nrow=1), mylegend, mycaption, nrow=3, heights=c(10, 1, 1.5))

ggsave(paste0('Lit_Review_Plot_t.',out_device), plot = p3_t, device = out_device, path = fig_path, width = 24, height = 36, units = "in", dpi=img_dpi)

rm(p1_t, p2_t, p3_t, mylegend, p1_t_legend)
