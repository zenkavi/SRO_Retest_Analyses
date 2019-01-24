fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

if(!exists('from_gh')){
  from_gh=FALSE
}

if(from_gh){
  require(RCurl)
}

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

tmp2 %>%
  group_by(contrast, rt_acc, raw_fit) %>%
  summarise(mean_icc = mean(icc2.1),
            sd_icc = sd(icc2.1, na.rm=T),
            n = n()) %>%
  mutate(cvl = qt(0.025, n-1),
         cvu = qt(0.975, n-1),
         cil = mean_icc+(sd_icc*cvl)/sqrt(n),
         ciu = mean_icc+(sd_icc*cvu)/sqrt(n),
         sem_icc = sd_icc/sqrt(n)) %>%
  ggplot(aes(factor(raw_fit, levels = c("raw", "EZ", "hddm"), labels=c("Raw", "EZ-diffusion", "Hierarchical diffusion")), mean_icc, fill=factor(rt_acc, levels = c("drift rate", "threshold", "non-decision","rt","accuracy"), labels=c("Drift Rate", "Threshold", "Non-decision","Response Time", "Accuracy"))))+
    geom_bar(stat="identity", position = position_dodge(width = 0.9), col="black")+
    geom_errorbar(aes(ymin=cil, ymax=ciu), position=position_dodge(width=0.9), width=0)+
  facet_wrap(~factor(contrast, levels=c("non-contrast", "contrast"), labels=c("Non-contrast", "Contrast")))+
  theme_bw()+
  ylab("Mean ICC")+
  xlab("")+
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=8),
        strip.text = element_text(size=8),
        axis.text = element_text(size = 8),
        text = element_text(size=8),
        legend.box.margin=margin(-15,-10,-10,-10),
        legend.key.size = unit(0.25,"cm"))+
  guides(fill = guide_legend(ncol = 2))+
  scale_fill_brewer(palette="Greys")

ggsave(paste0('PointEst_DDM_Comp.', out_device), device = out_device, path = fig_path, width = 7, height = 3.5, units = "in", limitsize = FALSE)
