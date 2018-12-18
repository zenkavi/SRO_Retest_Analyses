source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('lit_review')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/lit_review_data.R')
}

if(!exists('measure_labels')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/measure_labels_data.R')
}

if(!exists('boot_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/boot_rel_data.R')
}

tmp = as.character(unique(lit_review$dv)[which(unique(lit_review$dv) %in% meaningful_vars == FALSE)])
tmp = tmp[-grep('survey', tmp)]

tmp = measure_labels %>%
  mutate(dv = as.character(dv)) %>%
  left_join(boot_df[,c("dv", "icc2.1", "spearman")], by = 'dv')

tmp = tmp %>%
  separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
  mutate(task_group = factor(task_group, levels = task_group[order(task)])) %>%
  separate(var, c("var"), sep="\\.",remove=TRUE,extra="drop") %>%
  mutate(task_group = gsub("_", " ", task_group),
         var = gsub("_", " ", var)) %>%
  arrange(task_group, var)

tmp = tmp %>%
  left_join(rel_df[,c("dv", "icc2.1")], by = "dv") %>%
  rename(icc2.1 = icc2.1.x, point_est = icc2.1.y)

#Manual correction
tmp = tmp %>%
  mutate(task = ifelse(task_group == 'holt laury survey', "task", as.character(task))) %>%
  mutate(task_group = gsub("survey", "", task_group),
         task_group = gsub("task", "", task_group),
         task_group = str_to_title(task_group)) %>%
  mutate(task_group = ifelse(task_group == "Psychological Refractory Period Two Choices", "Psychological Refractory Period", ifelse(task_group == "Angling Risk Always Sunny", "Angling Risk", ifelse(task_group == "Two Stage", "Two Step", ifelse(task_group == "Threebytwo", "Task Switching", ifelse(task_group == "Adaptive N Back", "Adaptive N-back", ifelse(task_group == "Go Nogo", "Go/No-go",  ifelse(task_group == "Ravens", "Raven's", task_group)))))))) %>%
  mutate(task_group = ifelse(task_group == "Bis Bas ", "BIS-BAS", ifelse(task_group == "Bis11 ", "BIS-11", ifelse(task_group == "Dospert Eb ", "DOSPERT EB", ifelse(task_group == "Dospert Rp ", "DOSPOERT RP", ifelse(task_group == "Dospert Rt ", "DOSPERT RT", ifelse(task_group == "Erq ", "ERQ", ifelse(task_group == "Upps Impulsivity ", "UPPS-P", task_group))))))))

p4 <- tmp %>%
  filter(task == 'task',
         raw_fit == 'raw') %>%
  ggplot(aes(y = var, x = icc2.1)) +
  geom_point(color = '#00BFC4')+
  geom_point(aes(y = var, x = point_est), color = "black")+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y", labeller = label_wrap_gen(width=20)) +
  theme(panel.spacing = unit(0.75, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180, size=36),
        axis.text.y = element_text(size=20),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80")) +
  xlab("")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10))+
  geom_vline(xintercept = 0, color = "red", size = 1)

p5 <- tmp %>%
  filter(task == 'survey') %>%
  ggplot(aes(y = var, x = icc2.1)) +
  geom_point(color = '#F8766D')+
  geom_point(aes(y = var, x = point_est), color = "black")+
  facet_grid(task_group~., switch = "y", scales = "free_y", space = "free_y", labeller = label_wrap_gen(width=20)) +
  theme(panel.spacing = unit(0.75, "lines"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=180, size=36),
        axis.text.y = element_text(size=20),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey80")) +
  xlab("")+
  ylab("")+
  scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10))+
  geom_vline(xintercept = 0, color = "red", size = 1)

p6 <- arrangeGrob(p4, p5,nrow=1)

ggsave(paste0('Bootstrap_Raw_Var_Plot.', out_device), plot = p6, device = out_device, path = fig_path, width = 36, height = 72, units = "in", limitsize = FALSE, dpi=img_dpi)

rm(p4, p5, p6)