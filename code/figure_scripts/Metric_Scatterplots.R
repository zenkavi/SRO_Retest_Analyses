p1 = rel_df %>%
  ggplot(aes(spearman, icc, col=task))+
  geom_point()+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = "none")+
  geom_abline(intercept = 0, slope=1)

p2 = rel_df %>%
  ggplot(aes(pearson, icc, col=task))+
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

grid.arrange(p1, p2, p3, nrow=1)

ggsave('Metric_Scatterplots.jpg', plot = grid.arrange(p1, p2, p3, nrow=1), device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 12, height = 4, units = "in", limitsize = FALSE, dpi = 300)