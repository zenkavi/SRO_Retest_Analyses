if(from_gh){
  require(RCurl)
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('g_legend')){
  if(from_gh){
    eval(parse(text = getURL('https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/g_legend.R', ssl.verifypeer = FALSE)))
  }else{
    source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/g_legend.R')
  }
  
}

if(!exists('g_caption')){
  if(from_gh){
    eval(parse(text = getURL('https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/g_caption.R', ssl.verifypeer = FALSE)))
  }else{
    source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/g_caption.R')
  }
}

if(!exists('comp_lit_pred_out')){
  comp_lit_pred_out = read.csv('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/tables/comp_lit_pred_out.csv')
  
}


tmp = comp_lit_pred_out %>%
  select(-.n, -m_lit, -m_boot, -X) %>%
  gather(key, value) %>%
  separate(key, c("stat", "sample"), sep = "_")

# tmp$stat = as.factor(tmp$stat)
# levels(tmp$stat) <- c("coefficient", expression(r^"2"))

tmp %>%  
  ggplot(aes(value, fill=sample))+
  geom_density(alpha = 0.5, position='identity', color=NA)+
  # facet_grid(.~stat, scales='free', labeller = label_parsed)+
  scale_fill_manual(breaks=c("boot","lit"),
                    labels=c("Empirical", "Literature"),
                    name="Predictor",
                    values = c("gray15", "gray75"))+
  xlab('Proportion of Variance in Literature Explained')+
  ylab('Density')+
  xlim(0,1)+
  ylim(0,40)+
  theme(#axis.title.x  = element_text(size=12),
    #axis.text.x  = element_text(size=12),
    #axis.text.y  = element_text(size=12),
    #axis.title.y  = element_text(size=12),
    #legend.title  = element_text(size=12),
    #legend.text  = element_text(size=12),
    legend.position = "bottom",
    plot.caption = element_text(family = "Times", lineheight = 2, hjust = 0))+
  labs(caption = str_wrap("FIGURE 3: Noise ceiling for comparing empirical retest reliability estimates to reliability estimates from the literature. Data come from sampling a single reliability estimate from the literature and using that as a predictor of the remaining retest reliability estimates from the literature versus using the mean estimate from our empirical results as a predictor. Models account for the effect of sample size in the literature and whether the measure is a task or survey variables. The literature samples are significantly better at predicting the rest of the literature than our empirical averages, but the distributions of variance explained across samples are highly overlapping. ", width = 120))

ggsave(paste0('LitAndBoot_Noise_Ceiling.',out_device), device = out_device, path = fig_path, width = 7, height = 6, units = "in", dpi=img_dpi)
