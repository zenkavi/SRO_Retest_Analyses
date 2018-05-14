lit_review %>%
  filter(task_group == 'stroop') %>%
  ggplot(aes(y = var, x = retest_reliability)) +
  geom_point(aes(size=sample_size, shape = type), color='#00BFC4')+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180, size = 12),
        axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80"),
        legend.position = 'bottom') +
  xlab("")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_shape_manual(breaks = sort(lit_review$type), values = c(15, 16, 17, 3))
ggsave('Lit_Review_Example.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 12, height = 4, units = "in", dpi=300)



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

tmp %>%
  filter(task_group == "stroop") %>%
  ggplot(aes(x=factor(rank), y=value, fill=factor(key, levels = c("var_resid_pct", "var_ind_pct", "var_subs_pct"))))+
  geom_bar(stat='identity', alpha = 0.75, color='#00BFC4')+
  scale_x_discrete(breaks = labels$rank,
                   labels = labels$var)+
  coord_flip()+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.placement = "outside",
        strip.text.y = element_text(angle=180),
        axis.text.y = element_text(size = 11),
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
  xlab("")

ggsave('VarBreakdown_Example.jpg', device = "jpeg", path = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/", width = 12, height = 6, units = "in", dpi=300)

