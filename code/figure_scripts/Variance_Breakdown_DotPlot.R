source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('boot_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/boot_rel_data.R')
}

tmp = boot_df %>%
  select(dv, task, var_subs_pct, var_ind_pct, var_resid_pct)

p1 = tmp %>%
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
                    labels = c("Between subject variance",
                               "Within subject variance",
                               "Error variance")), mean_pct))+
  geom_errorbar(aes(ymin=cil, ymax=ciu, group=task), position=position_dodge(width = 0.25), width=0.25, size=2, alpha = 0.85, color="gray")+
  geom_point(position=position_dodge(width = 0.25), size=3, aes(color=task, shape=task))+
  #geom_bar(position=position_dodge(width = 0.5), width=0.5, aes(fill=task), stat='identity', alpha=0.5)+
  #geom_errorbar(aes(ymin=mean_pct-sd_pct, ymax=mean_pct+sd_pct), position=position_dodge(width = 0.25), width=0, size=2)+
  theme_bw()+
  xlab('')+
  ylab('Percent of total variance')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = 'bottom',
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.caption = element_text(family = "Times", lineheight = 2, hjust = 0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  ylim(0,100)+
  labs(caption = str_wrap("FIGURE 5:  Percentage of variance explained by each of the three sources of variance: between subjects, within subjects (between sessions) and error variance for 1000 bootstrapped samples. Error bars are 95% confidence intervals.", width = 120))

ggsave(paste0('Variance_Breakdown_DotPlot.', out_device), plot = p1, device = out_device, path = fig_path, width = 7, height = 4, units = "in", dpi = img_dpi)
