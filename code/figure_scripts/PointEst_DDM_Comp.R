source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('measure_labels')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/measure_labels_data.R')
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


tmp2 = measure_labels %>%
  mutate(dv = as.character(dv),
         contrast = ifelse(overall_difference == "difference", "contrast", "non-contrast")) %>%
  filter(ddm_task == 1,
         rt_acc != 'other') %>%
  drop_na() %>%
  left_join(rel_df[,c("dv", "icc2.1")], by = 'dv')

ddm_point_plot = tmp2 %>%
  ggplot(aes(factor(raw_fit, levels = c("raw", "EZ", "hddm"), labels=c("Raw", "EZ-diffusion", "Hierarchical diffusion")), icc2.1, fill=factor(rt_acc, levels = c("rt","accuracy", "drift rate", "threshold", "non-decision"), labels=c("Response Time", "Accuracy","Drift Rate", "Threshold", "Non-decision"))))+
  geom_boxplot()+
  facet_wrap(~factor(contrast, levels=c("non-contrast", "contrast"), labels=c("Non-contrast", "Contrast")))+
  theme_bw()+
  ylab("icc2.1")+
  xlab("")+
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=16),
        strip.text = element_text(size=16),
        axis.text = element_text(size = 16),
        text = element_text(size=16))+
  guides(fill = guide_legend(ncol = 2))+
  scale_fill_brewer(palette="Greys",
                    breaks=c( "Response Time", "Accuracy","Drift Rate", "Threshold", "Non-decision"))+
  ylim(-0.4, 1)

mylegend<-g_legend(ddm_point_plot)

grob_name <- names(mylegend$grobs)[1]

#manually fix the legend
#move non-decision down
#key
mylegend$grobs[grob_name][[1]]$layout[11,c(1:4)] <- c(4,8,4,8)
mylegend$grobs[grob_name][[1]]$layout[12,c(1:4)] <- c(4,8,4,8)
#text
mylegend$grobs[grob_name][[1]]$layout[17,c(1:4)] <- c(4,10,4,10)
#move threshold down
#key
mylegend$grobs[grob_name][[1]]$layout[9,c(1:4)] <- c(3,8,3,8)
mylegend$grobs[grob_name][[1]]$layout[10,c(1:4)] <- c(3,8,3,8)
#text
mylegend$grobs[grob_name][[1]]$layout[16,c(1:4)] <- c(3,10,3,10)
#move drift rate right and up
#key
mylegend$grobs[grob_name][[1]]$layout[7,c(1:4)] <- c(2,8,2,8)
mylegend$grobs[grob_name][[1]]$layout[8,c(1:4)] <- c(2,8,2,8)
#text
mylegend$grobs[grob_name][[1]]$layout[15,c(1:4)] <- c(2,10,2,10)

ddm_point_legend = arrangeGrob(ddm_point_plot +theme(legend.position="none"),
                               mylegend, nrow=2, heights=c(10, 1))

ggsave(paste0('PointEst_DDM_Comp.', out_device), plot=ddm_point_legend, device = out_device, path = fig_path, width = 15, height = 7, units = "in", limitsize = FALSE)

rm(ddm_point_legend, ddm_point_plot, mylegend, grob_name, tmp2)
