if(!exists('fig_path')){
  fig_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/figures/'
}

if(!exists('from_gh')){
  from_gh=FALSE
}

source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/figure_scripts/figure_res_wrapper.R')

if(!exists('lit_review')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/lit_review_data.R')
}

if(!exists('meaningful_vars')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/meaningful_vars_data.R')
}

if(!exists('measure_labels')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/measure_labels_data.R')
}

if(!exists('boot_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/boot_rel_data.R')
}

if(!exists('rel_df')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/subject_data.R')
  
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/helper_functions/make_rel_df.R')
  
  rel_df = make_rel_df(t1_df = test_data, t2_df = retest_data, metrics = c('spearman', 'icc2.1', 'pearson', 'var_breakdown', 'partial_eta', 'sem'))
  
  rel_df$task = 'task'
  rel_df[grep('survey', rel_df$dv), 'task'] = 'survey'
  rel_df[grep('holt', rel_df$dv), 'task'] = "task"
  rel_df = rel_df %>%
    select(dv, task, spearman, icc2.1, pearson, partial_eta, sem, var_subs, var_ind, var_resid)
}

require(grid)

tmp = as.character(unique(lit_review$dv)[which(unique(lit_review$dv) %in% meaningful_vars == FALSE)])
tmp = tmp[-grep('survey', tmp)]

meaningful_vars = c(meaningful_vars, tmp)
meaningful_vars = sort(meaningful_vars)

tmp = measure_labels %>%
  mutate(dv = as.character(dv)) %>%
  left_join(boot_df[,c("dv", "icc2.1", "spearman")], by = 'dv')

tmp = tmp %>%
  separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
  mutate(task_group = factor(task_group, levels = unique(task_group[order(task)]))) %>%
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
  mutate(task_group = ifelse(task_group == "Bis Bas ", "BIS-BAS", ifelse(task_group == "Bis11 ", "BIS-11", ifelse(task_group == "Dospert Eb ", "DOSPERT EB", ifelse(task_group == "Dospert Rp ", "DOSPERT RP", ifelse(task_group == "Dospert Rt ", "DOSPERT RT", ifelse(task_group == "Erq ", "ERQ", ifelse(task_group == "Upps Impulsivity ", "UPPS-P", task_group))))))))

tmp_mngfl = tmp %>%
  filter(dv %in% meaningful_vars)

#Extract trial number info to add to boot plot instead of adding another table

trial_num_info = tmp %>%
  group_by(task_group, task) %>%
  summarise(mean_num_all_trials = round(mean(num_all_trials)),
            num_measures = length(unique(dv)))

trial_num_info_mngfl = tmp_mngfl %>%
  group_by(task_group, task) %>%
  summarise(mean_num_all_trials = round(mean(num_all_trials)),
            num_measures = length(unique(dv)))

tmp = tmp %>%
  left_join(trial_num_info, by=c("task_group", "task"))

tmp_mngfl = tmp_mngfl %>%
  left_join(trial_num_info_mngfl, by=c("task_group", "task"))

#Boot plot for tasks with trial info (only using meaningful vars)

p4_t <- tmp_mngfl %>%
  filter(task == 'task') %>%
  ggplot(aes(x = factor(task_group, levels=rev(unique(task_group))), y = icc2.1)) +
  geom_violin()+
  geom_point(data = lit_review %>% filter(task == 'task'), aes(x = factor(task_group, levels=rev(unique(task_group))), y = retest_reliability), color="#E69F00", size=2) +
  theme(axis.text = element_text(size=6),
        plot.margin = unit(c(0,0,0,-0.25), "cm"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank())+
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1), position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_hline(yintercept = 0, color = "red", size = 1)+
  coord_flip()

task_trial_num = trial_num_info_mngfl %>%
  filter(task == "task") %>%
  ungroup() %>%
  select(-task)

task_trial_num_table = task_trial_num %>%
  mutate(y_axis = rev(ggplot_build(p4_t)$layout$panel_params[[1]]$y.major)) %>%
  gather(key, value, -y_axis, -task_group) %>%
  ggplot(aes(key, factor(y_axis)))+
  geom_text(aes(label=value), size=2)+
  xlab("")+
  ylab("")+
  theme(axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size=6),
        plot.margin = unit(c(0,-0.25,0,-0.65), "cm"))+
  scale_x_discrete(position = "top",
                   breaks=c("mean_num_all_trials", "num_measures"),
                   labels=c("Mean Tri", "Num Meas"))

boot_task_plot = arrangeGrob(p4_t, task_trial_num_table, nrow=1, widths = c(4,1.5), padding = unit(0, "line"))

ggsave(paste0('Task_Boot_w_trialinfo.', out_device), plot = boot_task_plot, device = out_device, path = fig_path, width = 3.5, height = 8.5, units = "in", limitsize = FALSE, dpi = img_dpi)

#Boot plot for surveys with trial info (only using meaningful vars)

p5_t <- tmp %>%
  filter(task == 'survey') %>%
  ggplot(aes(x = factor(task_group, levels=rev(unique(task_group))), y = icc2.1)) +
  geom_violin()+
  geom_point(data = lit_review %>% filter(task == 'survey'), aes(x = factor(task_group, levels=rev(unique(task_group))), y = retest_reliability), color="#56B4E9", size=2) +
  theme(axis.text = element_text(size=6),
        plot.margin = unit(c(0,0,0,-0.25), "cm"),
        panel.background = element_blank(),
        panel.border = element_blank())+
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1), position = "right")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  geom_hline(yintercept = 0, color = "red", size = 1)+
  coord_flip()

survey_trial_num = trial_num_info %>%
  filter(task == "survey") %>%
  ungroup() %>%
  select(-task)

survey_trial_num_table = survey_trial_num %>%
  mutate(y_axis = rev(ggplot_build(p5_t)$layout$panel_params[[1]]$y.major)) %>%
  gather(key, value, -y_axis, -task_group) %>%
  ggplot(aes(key, factor(y_axis)))+
  geom_text(aes(label=value), size=2)+
  xlab("")+
  ylab("")+
  theme(axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size=6),
        plot.margin = unit(c(0,-0.25,0,-0.65), "cm"))+
  scale_x_discrete(position = "top",
                   breaks=c("mean_num_all_trials", "num_measures"),
                   labels=c("Mean Tri", "Num Mes"))

boot_survey_plot = arrangeGrob(p5_t, survey_trial_num_table, nrow=1, widths = c(4,1.5), padding = unit(0, "line"))

ggsave(paste0('Survey_Boot_w_trialinfo.', out_device), plot = boot_survey_plot, device = out_device, path = fig_path, width = 3.5, height = 8.5, units = "in", limitsize = FALSE, dpi = img_dpi)

#Both task level boot plots with trial info together

boot_both_w_trial = arrangeGrob(boot_task_plot, boot_survey_plot, nrow=1, padding = unit(0, "line"))

ggsave(paste0('Boot_Both_w_trialinfo.', out_device), plot = boot_both_w_trial, device = out_device, path = fig_path, width = 7, height = 8.5, units = "in", limitsize = FALSE, dpi = img_dpi)

rm(tmp, tmp_mngfl, boot_survey_plot, boot_task_plot, boot_both_w_trial, task_trial_num_table, task_trial_num, survey_trial_num_table, survey_trial_num)
