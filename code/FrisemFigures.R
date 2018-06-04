lit_review %>%
  filter(task_group == 'stroop') %>%
  ggplot(aes(y = var, x = retest_reliability)) +
  geom_point(aes(size=sample_size, shape = type), color='#00BFC4')+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180, size = 36),
        axis.text.y = element_text(size = 26),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title  = element_text(size = 16),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80"),
        legend.position = 'bottom') +
  guides(shape = guide_legend(override.aes = list(size=16))) +
  xlab("")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_shape_manual(breaks = sort(lit_review$type), values = c(15, 16, 17, 3))
ggsave('Lit_Review_Example.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 12, height = 7, units = "in", dpi=500)

#######################################################
#######################################################
#######################################################


tmp %>%  
  ggplot(aes(value, fill=sample))+
  geom_density(alpha = 0.5, position='identity', color=NA)+
  # facet_grid(.~stat, scales='free', labeller = label_parsed)+
  scale_fill_manual(breaks=c("boot","lit"),
                    labels=c("Empirical", "Literature"),
                    name="Predictor",
                    values = c("white", "orange"))+
  xlab('Variance Explained')+
  ylab('Density')+
  xlim(0,1)+
  ylim(0,40)+
  theme(axis.title.x  = element_text(size=16),
        axis.text.x  = element_text(size=14),
        axis.title.y  = element_text(size=16),
        legend.title  = element_text(size=16),
        legend.text  = element_text(size=16))

ggsave('LitOnly_Noise_Ceiling.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 6, height = 4, units = "in", limitsize = FALSE, dpi = 500)

#######################################################
#######################################################
#######################################################


tmp = measure_labels %>%
  mutate(dv = as.character(dv)) %>%
  left_join(boot_df[,c("dv", "icc", "spearman")], by = 'dv')

tmp = tmp %>%
  separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
  mutate(task_group = factor(task_group, levels = task_group[order(task)])) %>%
  separate(var, c("var"), sep="\\.",remove=TRUE,extra="drop") %>%
  mutate(task_group = gsub("_", " ", task_group),
         var = gsub("_", " ", var)) %>%
  arrange(task_group, var)

tmp = tmp %>%
  left_join(rel_df[,c("dv", "icc")], by = "dv") %>%
  rename(icc = icc.x, point_est = icc.y)

#Manual correction
tmp = tmp %>%
  mutate(task = ifelse(task_group == 'holt laury survey', "task", as.character(task))) %>%
  mutate(task_group = ifelse(task_group == "psychological refractory period two choices", "psychological refractory period", ifelse(task_group == "angling risk task always sunny", "angling risk task",task_group))) %>%
  mutate(task_group = gsub("survey", "", task_group))

tmp %>%
  filter(task_group == 'stroop',
         raw_fit == 'raw') %>%
  ggplot(aes(y = var, x = icc)) +
  geom_point(color = '#00BFC4')+
  geom_point(aes(y = var, x = point_est), color = "black")+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y", labeller = label_wrap_gen(width=20)) +
  theme(panel.spacing = unit(0.75, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180, size=36),
        axis.text.y = element_text(size=26),
        axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=18),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80")) +
  xlab("ICC")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10))+
  geom_vline(xintercept = 0, color = "red", size = 1)

ggsave('Boot_Example.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 12, height = 8, units = "in", dpi=500)

#######################################################
#######################################################
#######################################################

tmp = rel_df %>%
  mutate(var_subs_pct = var_subs/(var_subs+var_ind+var_resid)*100,
         var_ind_pct = var_ind/(var_subs+var_ind+var_resid)*100, 
         var_resid_pct = var_resid/(var_subs+var_ind+var_resid)*100) %>%
  select(dv, task, var_subs_pct, var_ind_pct, var_resid_pct) %>%
  mutate(dv = factor(dv, levels = dv[order(task)])) %>%
  separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
  mutate(task_group = factor(task_group, levels = task_group[order(task)])) %>%
  arrange(task_group, var_subs_pct) %>%
  mutate(rank = row_number()) %>%
  arrange(task, task_group, rank) %>%
  gather(key, value, -dv, -task_group, -var, -task, -rank) %>%
  ungroup()%>%
  mutate(task_group = gsub("_", " ", task_group),
         var = gsub("_", " ", var)) %>%
  mutate(task_group = ifelse(task_group == "psychological refractory period two choices", "psychological refractory period", ifelse(task_group == "angling risk task always sunny", "angling risk task",task_group))) %>%
  mutate(task_group = gsub("survey", "", task_group))
  
labels = tmp %>%
  distinct(dv, .keep_all=T)

tmp = tmp %>% 
  left_join(measure_labels[,c("dv", "raw_fit")])

tmp %>%
  filter(task_group == "stroop", raw_fit == "raw") %>%
  ggplot(aes(x=factor(rank), y=value, fill=factor(key, levels = c("var_resid_pct", "var_ind_pct", "var_subs_pct"))))+
  geom_bar(stat='identity', alpha = 0.75, color='#00BFC4')+
  scale_x_discrete(breaks = labels$rank,
                   labels = labels$var)+
  coord_flip()+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.placement = "outside",
        strip.text.y = element_text(angle=180, size = 36),
        axis.text.y = element_text(size = 26),
        axis.text.x = element_text(size  = 20),
        legend.text = element_text(size = 24),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey85"),
        legend.position = 'bottom')+
  theme(legend.title = element_blank())+
  scale_fill_manual(breaks = c("var_subs_pct", "var_ind_pct", "var_resid_pct"),
                    labels = c("Variance between individuals",
                               "Variance between sessions",
                               "Error variance"),
                    values=c("grey65", "grey45", "grey25"))+
  ylab("")+
  xlab("")+
  guides(fill = guide_legend(ncol = 1))

ggsave('VarBreakdown_Example.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", 
       width = 14, height = 10, units = "in", dpi=400)

#######################################################
#######################################################
#######################################################

tmp = duplicate_items_data_t1 %>%
  gather(key, value, -worker)

tmp2 = duplicate_items_data_t2 %>%
  gather(key, value, -worker)

tmp = duplicate_items %>%
  select(item1_ID, item2_ID) %>%
  left_join(tmp, by = c("item1_ID" = "key")) %>%
  left_join(tmp, by = c("item2_ID" = "key", "worker" = "worker")) %>%
  rename(item1_time1 = value.x, item2_time1 = value.y) %>%
  left_join(tmp2, by = c("item1_ID" = "key", "worker" = "worker")) %>%
  left_join(tmp2, by = c("item2_ID" = "key", "worker" = "worker")) %>%
  rename(item1_time2 = value.x, item2_time2 = value.y) %>%
  group_by(item1_ID) %>%
  mutate(item1_time1 = scale(item1_time1),
         item1_time2 = scale(item1_time2),
         item2_time1 = scale(item2_time1),
         item2_time2 = scale(item2_time2),
         item1_time1 = ifelse(item1_ID == "mpq_control_survey.13", item1_time1*-1, item1_time1),
         item1_time2 = ifelse(item1_ID == "mpq_control_survey.13", item1_time2*-1, item1_time2)) %>%
  ungroup()

p1 = tmp %>%
  filter(grepl("bis_bas", item1_ID)) %>%
  ggplot(aes(item1_time1, item2_time1, col=item1_ID))+
  geom_smooth (alpha=0.3, size=0, span=0.5, method = "lm")+
  stat_smooth (geom="line", alpha=1, size=1, span=0.5, method= "lm")+
  theme(legend.position = "none")+
  geom_abline(slope=1, intercept=0, size = 2, linetype = "dashed")+
  xlab("BIS BAS T1")+
  ylab("BIS-11 T1")+
  ylim(-1,2)

p2 = tmp %>%
  filter(grepl("bis_bas", item1_ID)) %>%
  ggplot(aes(item1_time1, item2_time2, col=item1_ID))+
  geom_smooth (alpha=0.3, size=0, span=0.5, method = "lm")+
  stat_smooth (geom="line", alpha=1, size=1, span=0.5, method= "lm")+
  theme(legend.position = "none")+
  geom_abline(slope=1, intercept=0, size = 2, linetype = "dashed")+
  xlab("BIS BAS T1")+
  ylab("BIS-11 T2")+
  ylim(-1,2)

p3 = tmp %>%
  filter(grepl("bis_bas", item1_ID)) %>%
  ggplot(aes(item1_time2, item2_time1, col=item1_ID))+
  geom_smooth (alpha=0.3, size=0, span=0.5, method = "lm")+
  stat_smooth (geom="line", alpha=1, size=1, span=0.5, method= "lm")+
  theme(legend.position = "none")+
  geom_abline(slope=1, intercept=0, size = 2, linetype = "dashed")+
  xlab("BIS BAS T2")+
  ylab("BIS-11 T1")+
  ylim(-1,2)

p4 = tmp %>%
  filter(grepl("bis_bas", item1_ID)) %>%
  ggplot(aes(item1_time2, item2_time2, col=item1_ID))+
  geom_smooth (alpha=0.3, size=0, span=0.5, method = "lm")+
  stat_smooth (geom="line", alpha=1, size=1, span=0.5, method= "lm")+
  theme(legend.position = "none")+
  geom_abline(slope=1, intercept=0, size = 2, linetype = "dashed")+
  xlab("BIS BAS T2")+
  ylab("BIS-BAS T2")

p5 = arrangeGrob(p1, p2, p3, p4, ncol=2)

ggsave('DataCheckOverlappingItemsExample.jpg', p5, device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 8, height = 5, units = "in", limitsize = FALSE, dpi = 500)

rm(tmp, tmp2, p1, p2, p3, p4, p5)

#######################################################
#######################################################
#######################################################

rel_df %>%
  ggplot(aes(icc, fill=task))+
  geom_density(alpha = 0.5, color=NA)+
  xlim(-0.4,1)+
  xlab("ICC")+
  ylab("Density")+
  theme(legend.title = element_blank())+
  scale_fill_manual(values = c("white", "#00BFC4"))

ggsave('TaskVarsPointEstDist.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 7, height = 5, units = "in", limitsize = FALSE, dpi = 300)

rel_df %>%
  ggplot(aes(icc, fill=factor(task, levels = c("task", "survey"), labels = c("task", "survey"))))+
  geom_density(alpha = 0.5, color=NA)+
  xlim(-0.4,1)+
  xlab("ICC")+
  ylab("Density")+
  theme(legend.title = element_blank())+
  scale_fill_manual(values = c("white", "#F8766D"))

ggsave('SurveyVarsPointEstDist.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 7, height = 5, units = "in", limitsize = FALSE, dpi = 300)

rel_df %>%
  ggplot(aes(icc, fill=task))+
  geom_density(alpha = 0.5, color=NA)+
  xlim(-0.4,1)+
  xlab("ICC")+
  ylab("Density")+
  theme(legend.title = element_blank())

ggsave('AllVarsPointEstDist.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 7, height = 5, units = "in", limitsize = FALSE, dpi = 300)


#######################################################
#######################################################
#######################################################
rel_df %>%
  ggplot(aes(task, icc, fill=task))+
  geom_boxplot()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=14))+
  xlab("")+
  ylab("Mean ICC")+
  ylim(-0.4,1)
ggsave('TaskVsSurvey.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 3, height = 5, units = "in", limitsize = FALSE, dpi = 500)
