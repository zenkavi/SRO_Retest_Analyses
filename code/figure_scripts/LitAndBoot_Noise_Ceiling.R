fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

if(!exists('from_gh')){
  from_gh=FALSE
}

if(from_gh){
  require(RCurl)
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('comp_lit_pred_out')){
  comp_lit_pred_out = read.csv('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/tables/comp_lit_pred_out.csv')
  
}

tmp = comp_lit_pred_out %>%
  select(-.n, -m_lit, -m_boot, -X) %>%
  gather(key, value) %>%
  separate(key, c("stat", "sample"), sep = "_")

tmp %>%  
  ggplot(aes(value, fill=sample))+
  geom_density(alpha = 0.5, position='identity', color=NA)+
  scale_fill_manual(breaks=c("boot","lit"),
                    labels=c("Empirical", "Literature"),
                    name="Predictor",
                    values = c("gray15", "gray75"))+
  xlab('Proportion of Variance\nin Literature Explained')+
  ylab('Density')+
  xlim(0,1)+
  theme(legend.position = "bottom",
    legend.box.margin=margin(-10,-10,-10,-10),
    legend.key.size = unit(0.25,"cm"))

ggsave(paste0('LitAndBoot_Noise_Ceiling.',out_device), device = out_device, path = fig_path, width = 3.4, height = 3, units = "in", dpi=img_dpi)
