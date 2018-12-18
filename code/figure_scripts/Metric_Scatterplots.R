source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('rel_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/subject_data.R')
  
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/make_rel_df.R')
  
  rel_df = make_rel_df(t1_df = test_data, t2_df = retest_data, metrics = c('spearman', 'icc2.1','icc3.k', 'pearson', 'var_breakdown', 'partial_eta', 'sem'))
  
  rel_df$task = 'task'
  rel_df[grep('survey', rel_df$dv), 'task'] = 'survey'
  rel_df[grep('holt', rel_df$dv), 'task'] = "task"
  rel_df = rel_df %>%
    select(dv, task, spearman, icc, pearson, partial_eta, sem, var_subs, var_ind, var_resid)
}

p1 = rel_df %>%
  ggplot(aes(spearman, icc2.1, col=task))+
  geom_point()+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_abline(intercept = 0, slope=1)

p2 = rel_df %>%
  ggplot(aes(pearson, icc2.1, col=task))+
  geom_point()+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_abline(intercept = 0, slope=1)

p3 = rel_df %>%
  ggplot(aes(pearson, spearman, col=task))+
  geom_point()+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_abline(intercept = 0, slope=1)

p4 = rel_df %>%
  ggplot(aes(spearman, icc3.k, col=task))+
  geom_point()+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_abline(intercept = 0, slope=1)

p5 = rel_df %>%
  ggplot(aes(pearson, icc3.k, col=task))+
  geom_point()+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_abline(intercept = 0, slope=1)

p6 = rel_df %>%
  ggplot(aes(icc2.1, icc3.k, col=task))+
  geom_point()+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_abline(intercept = 0, slope=1)

ggsave(paste0('Metric_Scatterplots.',out_device), plot = grid.arrange(p1, p2, p3, p4, p5, p6, nrow=2), device = out_device, path = fig_path, width = 12, height = 8, units = "in", dpi=img_dpi)

rm(p1,p2,p3,p4,p5,p6)
