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
  mutate(task_group = gsub("survey", "", task_group),
         task_group = gsub("task", "", task_group),
         task_group = str_to_title(task_group)) %>%
  mutate(task_group = ifelse(task_group == "Psychological Refractory Period Two Choices", "Psychological Refractory Period", ifelse(task_group == "Angling Risk Always Sunny", "Angling Risk", ifelse(task_group == "Two Stage", "Two Step", ifelse(task_group == "Threebytwo", "Task Switching", ifelse(task_group == "Adaptive N Back", "Adaptive N-back", ifelse(task_group == "Go Nogo", "Go/No-go",  ifelse(task_group == "Ravens", "Raven's", task_group)))))))) %>%
  mutate(task_group = ifelse(task_group == "Bis Bas ", "BIS-BAS", ifelse(task_group == "Bis11 ", "BIS-11", ifelse(task_group == "Dospert Eb ", "DOSPERT EB", ifelse(task_group == "Dospert Rp ", "DOSPOERT RP", ifelse(task_group == "Dospert Rt ", "DOSPERT RT", ifelse(task_group == "Erq ", "ERQ", ifelse(task_group == "Upps Impulsivity ", "UPPS-P", task_group))))))))

p1<- tmp %>%
  filter(grepl('selection_optimization', dv)) %>%
  ggplot(aes(x = var, y = icc)) +
  geom_violin(fill='#F8766D')+
  theme_bw() +
  theme(axis.text.y = element_text(size=30))+
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_hline(yintercept = 0, color = "red", size = 1)+
  coord_flip()

p2<- tmp %>%
  filter(grepl('selection_optimization', dv)) %>%
  ggplot(aes(x = factor(task_group, levels=rev(unique(task_group))), y = icc, group=dv)) +
  geom_violin(fill='#F8766D', position = 'identity')+
  theme_bw() +
  theme(axis.text.y = element_text(size=30))+
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_hline(yintercept = 0, color = "red", size = 1)+
  coord_flip()

p3<- tmp %>%
  filter(grepl('selection_optimization', dv)) %>%
  ggplot(aes(x = factor(task_group, levels=rev(unique(task_group))), y = icc)) +
  geom_violin(fill='#F8766D', position = 'identity')+
  theme_bw() +
  theme(axis.text.y = element_text(size=30))+
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_hline(yintercept = 0, color = "red", size = 1)+
  coord_flip()

p4 <- arrangeGrob(p1,p2, p3,nrow=1)

ggsave(paste0('Bootstrap_Example_Plot_t.', out_device), plot = p4, device = out_device, path = fig_path, width = 41, height = 5, units = "in", limitsize = FALSE, dpi = img_dpi)

rm(p1, p2, p3, p4)
