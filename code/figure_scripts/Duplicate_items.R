fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

if(!exists('from_gh')){
  from_gh=FALSE
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('duplicate_items')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/duplicate_items_data.R')
}

require(psych)

extract_items = c('worker',unique(with(duplicate_items, c(item1_ID, item2_ID))))

duplicate_items_data_t1 = duplicate_items_data_t1 %>%
  filter(worker %in% duplicate_items_data_t2$worker) %>%
  select(extract_items)

duplicate_items_data_t2=duplicate_items_data_t2 %>%
  filter(worker %in% duplicate_items_data_t1$worker) %>%
  select(extract_items)

duplicate_items = duplicate_items %>%
  mutate(t1_t1_cor = NA,
         t2_t2_cor = NA,
         t1_t2_cor = NA,
         t2_t1_cor = NA,
         t1_t1_polycor = NA,
         t2_t2_polycor = NA,
         t1_t2_polycor = NA,
         t2_t1_polycor = NA)

for(i in 1:nrow(duplicate_items)){
  duplicate_items$t1_t1_cor[i] = abs(cor(duplicate_items_data_t1[,c(duplicate_items$item1_ID[i])],
                                         duplicate_items_data_t1[,c(duplicate_items$item2_ID[i])]))
  
  duplicate_items$t2_t2_cor[i] = abs(cor(duplicate_items_data_t2[,c(duplicate_items$item1_ID[i])],
                                         duplicate_items_data_t2[,c(duplicate_items$item2_ID[i])]))
  
  duplicate_items$t1_t2_cor[i] = abs(cor(duplicate_items_data_t1[,c(duplicate_items$item1_ID[i])],
                                         duplicate_items_data_t2[,c(duplicate_items$item2_ID[i])]))
  
  duplicate_items$t2_t1_cor[i] = abs(cor(duplicate_items_data_t2[,c(duplicate_items$item1_ID[i])],
                                         duplicate_items_data_t1[,c(duplicate_items$item2_ID[i])]))
  
  duplicate_items$t1_t1_polycor[i] = abs(polychoric(data.frame(duplicate_items_data_t1[,c(duplicate_items$item1_ID[i])],
                                                               duplicate_items_data_t1[,c(duplicate_items$item2_ID[i])]))$rho[2])
  
  duplicate_items$t2_t2_polycor[i] = abs(polychoric(data.frame(duplicate_items_data_t2[,c(duplicate_items$item1_ID[i])],
                                                               duplicate_items_data_t2[,c(duplicate_items$item2_ID[i])]))$rho[2])
  
  duplicate_items$t1_t2_polycor[i] = abs(polychoric(data.frame(duplicate_items_data_t1[,c(duplicate_items$item1_ID[i])],
                                                               duplicate_items_data_t2[,c(duplicate_items$item2_ID[i])]))$rho[2])
  
  duplicate_items$t2_t1_polycor[i] = abs(polychoric(data.frame(duplicate_items_data_t2[,c(duplicate_items$item1_ID[i])],
                                                               duplicate_items_data_t1[,c(duplicate_items$item2_ID[i])]))$rho[2])
}

for(i in 1:nrow(duplicate_items)){
  duplicate_items$t1_t1_cor[i] = abs(cor(scale(duplicate_items_data_t1[,c(duplicate_items$item1_ID[i])]),
                                         scale(duplicate_items_data_t1[,c(duplicate_items$item2_ID[i])])))
  
  duplicate_items$t2_t2_cor[i] = abs(cor(scale(duplicate_items_data_t2[,c(duplicate_items$item1_ID[i])]),
                                         scale(duplicate_items_data_t2[,c(duplicate_items$item2_ID[i])])))
  
  duplicate_items$t1_t2_cor[i] = abs(cor(scale(duplicate_items_data_t1[,c(duplicate_items$item1_ID[i])]),
                                         scale(duplicate_items_data_t2[,c(duplicate_items$item2_ID[i])])))
  
  duplicate_items$t2_t1_cor[i] = abs(cor(scale(duplicate_items_data_t2[,c(duplicate_items$item1_ID[i])]),
                                         scale(duplicate_items_data_t1[,c(duplicate_items$item2_ID[i])])))
  
  duplicate_items$t1_t1_polycor[i] = abs(polychoric(data.frame(duplicate_items_data_t1[,c(duplicate_items$item1_ID[i])],
                                                               duplicate_items_data_t1[,c(duplicate_items$item2_ID[i])]))$rho[2])
  
  duplicate_items$t2_t2_polycor[i] = abs(polychoric(data.frame(duplicate_items_data_t2[,c(duplicate_items$item1_ID[i])],
                                                               duplicate_items_data_t2[,c(duplicate_items$item2_ID[i])]))$rho[2])
  
  duplicate_items$t1_t2_polycor[i] = abs(polychoric(data.frame(duplicate_items_data_t1[,c(duplicate_items$item1_ID[i])],
                                                               duplicate_items_data_t2[,c(duplicate_items$item2_ID[i])]))$rho[2])
  
  duplicate_items$t2_t1_polycor[i] = abs(polychoric(data.frame(duplicate_items_data_t2[,c(duplicate_items$item1_ID[i])],
                                                               duplicate_items_data_t1[,c(duplicate_items$item2_ID[i])]))$rho[2])
}

duplicate_items %>%
  arrange(-t1_t1_polycor, -t2_t2_polycor)

tmp = duplicate_items %>%
  select(similarity, t1_t1_polycor, t2_t2_polycor) %>%
  gather(key, value, -similarity)

tmp %>%
  ggplot(aes(similarity, value, col=key))+
  geom_smooth(method="lm", alpha = 0.15)+
  ylab("Polychoric correlation")+
  xlab("Levenshtein distance")+
  scale_color_discrete(breaks = c("t1_t1_polycor", "t2_t2_polycor"),
                       labels = c("T1 correlations", "T2 correlations"),
                       name = element_blank())+
  theme(legend.text = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(size=10))

ggsave(paste0('Duplicate_items.', out_device), device = out_device, path = fig_path, width = 3.4, height = 3, units = "in", limitsize = FALSE, dpi = img_dpi)