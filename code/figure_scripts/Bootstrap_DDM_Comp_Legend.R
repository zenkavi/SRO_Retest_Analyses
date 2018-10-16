source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('measure_labels')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/measure_labels_data.R')
}

if(!exists('boot_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/boot_rel_data.R')
}

tmp = measure_labels %>%
  mutate(dv = as.character(dv),
         contrast = ifelse(overall_difference == "difference", "contrast", "non-contrast")) %>%
  filter(ddm_task == 1,
         rt_acc != 'other') %>%
  drop_na() %>%
  left_join(boot_df[,c("dv", "icc")], by = 'dv')

ddm_boot_plot = tmp %>%
  group_by(dv) %>%
  summarise(mean_icc = mean(icc),
            raw_fit = unique(raw_fit),
            contrast = unique(contrast),
            rt_acc = unique(rt_acc)) %>%
  ggplot(aes(factor(raw_fit, levels = c("raw", "EZ", "hddm"), labels=c("Raw", "EZ-diffusion", "Hierarchical diffusion")), mean_icc, fill=factor(rt_acc, levels = c("rt","accuracy", "drift rate", "threshold", "non-decision"), labels=c("Response Time", "Accuracy","Drift Rate", "Threshold", "Non-decision"))))+
  geom_boxplot()+
  facet_wrap(~factor(contrast, levels=c("non-contrast", "contrast"), labels=c("Non-contrast", "Contrast")))+
  theme_bw()+
  ylab("ICC")+
  xlab("")+
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=16),
        strip.text = element_text(size=16),
        axis.text = element_text(size = 16),
        text = element_text(size=16))+
  guides(fill = guide_legend(ncol = 2, byrow=F))+
  scale_fill_brewer(palette="Greys",
                    breaks=c( "Response Time", "Accuracy","Drift Rate", "Threshold", "Non-decision"))

mylegend<-g_legend(ddm_boot_plot)

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

mycaption = textGrob(label = str_wrap("FIGURE 7: Average bootstrapped reliability estimates per measure comparing raw measures and model parameters as well as contrast and non-contrast measures for task measures.", width = 130), gp = gpar(fontfamily="Times", lineheight = 2, fontsize = 18), hjust = 0, vjust = 0.5, x = unit(0, "npc"))

ddm_boot_legend = arrangeGrob(ddm_boot_plot +theme(legend.position="none"),
                              mylegend, mycaption, nrow=3, heights=c(10, 1, 1))

ggsave(paste0('Bootstrap_DDM_Comp_Legend.', out_device), plot = ddm_boot_legend, device = , path = fig_path, width = 14, height = 10, units = "in", limitsize = FALSE, dpi=img_dpi)
