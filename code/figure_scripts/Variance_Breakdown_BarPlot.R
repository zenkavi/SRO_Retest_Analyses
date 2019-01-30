fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'

if(!exists('from_gh')){
  from_gh=FALSE
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('rel_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/subject_data.R')
  
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/make_rel_df.R')
  
  rel_df = make_rel_df(t1_df = test_data, t2_df = retest_data, metrics = c('spearman', 'icc2.1', 'pearson', 'var_breakdown', 'partial_eta', 'sem'))
  
  rel_df$task = 'task'
  rel_df[grep('survey', rel_df$dv), 'task'] = 'survey'
  rel_df[grep('holt', rel_df$dv), 'task'] = "task"
  rel_df = rel_df %>%
    select(dv, task, spearman, icc2.1, pearson, partial_eta, sem, var_subs, var_ind, var_resid) %>% mutate(var_subs_pct = var_subs/(var_subs+var_ind+var_resid)*100,
                                                                                                           var_ind_pct = var_ind/(var_subs+var_ind+var_resid)*100,
                                                                                                           var_resid_pct = var_resid/(var_subs+var_ind+var_resid)*100)
}

tmp = rel_df %>%
  select(dv, task, var_subs_pct, var_ind_pct, var_resid_pct)

p1 = tmp %>%
  mutate(task = factor(task, levels = c("task", "survey"), labels = c("Task", "Survey"))) %>%
  gather(key, value, -dv, -task) %>%
  group_by(task, key) %>%
  summarise(mean_pct = mean(value),
            sd_pct = sd(value, na.rm=T),
            n = n()) %>%
  mutate(cvl = qt(0.025, n-1),
         cvu = qt(0.975, n-1),
         cil = mean_pct+(sd_pct*cvl)/sqrt(n),
         ciu = mean_pct+(sd_pct*cvu)/sqrt(n),
         sem_pct = sd_pct/sqrt(n)) %>%
  ggplot(aes(factor(key, levels = c("var_subs_pct", "var_ind_pct", "var_resid_pct"),
                    labels = c("Between subject",
                               "Within subject",
                               "Error variance")), mean_pct))+
  geom_bar(position=position_dodge(width = 0.5), width=0.5, aes(fill=task), stat='identity', alpha=0.5)+
  geom_errorbar(aes(ymin=cil, ymax=ciu, col=task), position=position_dodge(width = 0.5), width=0, size=2)+
  theme_bw()+
  xlab('')+
  ylab('% of total variance')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.position = c(.87, .8),
        axis.text = element_text(size=8),
        axis.title.y = element_text(size=8),
        legend.key.size = unit(0.25,"cm"),
        panel.grid = element_blank(),
        plot.margin = margin(.25,.25,-.5,.25, "cm"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100.3))

ggsave(paste0('Variance_Breakdown_BarPlot.', out_device), plot = p1, device = out_device, path = fig_path, width = 3.4, height = 1.5, units = "in", dpi = img_dpi)
