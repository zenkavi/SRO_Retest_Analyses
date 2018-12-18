source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('rel_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/subject_data.R')
  
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/make_rel_df.R')
  
  rel_df = make_rel_df(t1_df = test_data, t2_df = retest_data, metrics = c('spearman', 'icc', 'pearson', 'var_breakdown', 'partial_eta', 'sem'))
  
  rel_df$task = 'task'
  rel_df[grep('survey', rel_df$dv), 'task'] = 'survey'
  rel_df[grep('holt', rel_df$dv), 'task'] = "task"
  rel_df = rel_df %>%
    select(dv, task, spearman, icc, pearson, partial_eta, sem, var_subs, var_ind, var_resid)
}

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
  mutate(task_group = gsub("survey", "", task_group)) %>%
  filter(task=="task",
         !grepl("EZ|hddm", dv))%>%
  arrange(task_group, rank)
labels = tmp %>%
  distinct(dv, .keep_all=T)

p1 <- tmp %>%
  ggplot(aes(x=factor(rank), y=value, fill=factor(key, levels = c("var_resid_pct", "var_ind_pct", "var_subs_pct"))))+
  geom_bar(stat='identity', alpha = 0.75, color='#00BFC4')+
  scale_x_discrete(breaks = labels$rank,
                   labels = labels$var)+
  coord_flip()+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180),
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
  mutate(task_group = gsub("survey", "", task_group)) %>%
  filter(task=="survey")%>%
  arrange(task_group, rank)
labels = tmp %>%
  distinct(dv, .keep_all=T)

p2 <- tmp %>%
  ggplot(aes(x=factor(rank), y=value, fill=factor(key, levels = c("var_resid_pct", "var_ind_pct", "var_subs_pct"))))+
  geom_bar(stat='identity', alpha = 0.75)+
  geom_bar(stat='identity', color='#F8766D', show.legend=FALSE)+
  scale_x_discrete(breaks = labels$rank,
                   labels = labels$var)+
  coord_flip()+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180),
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

mylegend<-g_legend(p2)

p3 <- arrangeGrob(arrangeGrob(p1 +theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 1))

ggsave(paste0('Variance_Breakdown_Plot.', out_device), plot = p3, device = out_device, path = fig_path, width = 24, height = 20, units = "in", dpi = img_dpi)

rm(tmp, labels, p1, p2 , p3)
