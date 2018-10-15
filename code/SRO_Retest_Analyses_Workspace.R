# Load data

library(tidyverse)
library(jsonlite)

test_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Complete_03-29-2018/'

retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'

#########################
## Battery completion data ####
#########################

workers = read.csv(paste0(retest_data_path,'Local/User_717570_workers.csv'))
workers = workers %>%
  group_by(Worker.ID) %>%
  mutate(Retest_worker=ifelse(sum(CURRENT.RetestWorker,CURRENT.RetestWorkerB2,CURRENT.RetestWorkerB3,CURRENT.RetestWorkerB4,CURRENT.RetestWorkerB5,na.rm=T)>0,1,0)) %>%
  ungroup()

worker_counts <- fromJSON(paste0(retest_data_path,'/Local/retest_worker_counts.json'))

worker_counts = as.data.frame(unlist(worker_counts))
names(worker_counts) = "task_count"

disc_comp_date = read.csv(paste0(retest_data_path,'Local/discovery_completion_dates.csv'), header=FALSE)
val_comp_date = read.csv(paste0(retest_data_path,'Local/validation_completion_dates.csv'), header=FALSE)
test_comp_date = rbind(disc_comp_date, val_comp_date)
rm(disc_comp_date, val_comp_date)
retest_comp_date = read.csv(paste0(retest_data_path,'Local/retest_completion_dates.csv'), header=FALSE)
comp_dates = merge(retest_comp_date, test_comp_date, by="V1")
names(comp_dates) <- c("sub_id", "retest_comp", "test_comp")
comp_dates$retest_comp = as.Date(comp_dates$retest_comp)
comp_dates$test_comp = as.Date(comp_dates$test_comp)
comp_dates$days_btw = with(comp_dates, retest_comp-test_comp)

rm(test_comp_date, retest_comp_date, comp_dates)

### Completion time data

retest_task_comp_times = read.csv(paste0(retest_data_path, 'Local/retest_task_completion_times.csv'))
test_task_comp_times = read.csv(paste0(retest_data_path, 'Local/test_task_completion_times.csv'))
task_comp_times = merge(retest_task_comp_times, test_task_comp_times, by=c('worker_id','task'))
rm(retest_task_comp_times, test_task_comp_times)
task_comp_times = task_comp_times %>%
  select(-X.x, -X.y) %>%
  mutate(finish_day.x = as.Date(finish_day.x),
         finish_day.y = as.Date(finish_day.y),
         days_btw = finish_day.x-finish_day.y) %>%
  rename(sub_id=worker_id)

#########################
## Literature review data ####
#########################

lit_review <- read.csv('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/lit_review_figure.csv')

lit_review = lit_review %>%
  separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
  mutate(task_group = factor(task_group, levels = task_group[order(task)]),
         type = as.character(type)) %>%
  mutate(task_group = gsub("_", " ", task_group),
         var = gsub("_", " ", var)) %>%
  arrange(task_group, raw_fit, var) %>%
  mutate(task_group = gsub("survey", "", task_group),
         task_group = gsub("task", "", task_group),
         task_group = str_to_title(task_group)) %>%
  mutate(task_group = ifelse(task_group == "Psychological Refractory Period Two Choices", "Psychological Refractory Period", ifelse(task_group == "Angling Risk Always Sunny", "Angling Risk", ifelse(task_group == "Two Stage", "Two Step", ifelse(task_group == "Threebytwo", "Task Switching", ifelse(task_group == "Adaptive N Back", "Adaptive N-back", ifelse(task_group == "Go Nogo", "Go/No-go",  ifelse(task_group == "Ravens", "Raven's", task_group)))))))) %>%
  mutate(task_group = ifelse(task_group == "Bis Bas ", "BIS-BAS", ifelse(task_group == "Bis11 ", "BIS-11", ifelse(task_group == "Dospert Eb ", "DOSPERT EB", ifelse(task_group == "Dospert Rp ", "DOSPOERT RP", ifelse(task_group == "Dospert Rt ", "DOSPERT RT", ifelse(task_group == "Erq ", "ERQ", ifelse(task_group == "Upps Impulsivity ", "UPPS-P", task_group))))))))  %>%
  select(-measure_description)

#########################
## Subject data ####
#########################

#Get variables of interest from Ian's release
tmp1 <- read.csv(paste0(test_data_path,'meaningful_variables.csv'))
tmp2 <- read.csv(paste0(test_data_path,'meaningful_variables_noDDM.csv'))
tmp3 <- read.csv(paste0(test_data_path,'meaningful_variables_EZ.csv'))
retest_report_vars = c(names(tmp1), names(tmp2), names(tmp3))
retest_report_vars = unique(retest_report_vars)
lit_rev_vars = as.character(unique(lit_review$dv)[which(unique(lit_review$dv) %in% retest_report_vars == FALSE)])
retest_report_vars = c(retest_report_vars, lit_rev_vars)
rm(tmp1, tmp2, tmp3, lit_rev_vars)


### Load time 1 data

test_data <- read.csv(paste0(retest_data_path,'t1_data/variables_exhaustive.csv'))

test_data <- test_data[,names(test_data) %in% retest_report_vars]

test_data$X <- as.character(test_data$X)
names(test_data)[which(names(test_data) == 'X')] <-'sub_id'

### Load time 2 data

retest_data <- read.csv(paste0(retest_data_path,'variables_exhaustive.csv'))

retest_data <- retest_data[,names(retest_data) %in% retest_report_vars]

retest_data$X <- as.character(retest_data$X)
names(retest_data)[which(names(retest_data) == 'X')] <-'sub_id'
retest_data = retest_data[retest_data$sub_id %in% test_data$sub_id,]

### Replace HDDM parameters in t1 data

# Since HDDM parameters depend on the sample on which they are fit we refit the model on t1 data for the subjects that have t2 data. Here we replace the HDDM parameters in the current t1 dataset with these refitted values.

hddm_refits <- read.csv(paste0(retest_data_path,'t1_data/hddm_refits_exhaustive.csv'))

hddm_refits = hddm_refits[,names(hddm_refits) %in% retest_report_vars]

hddm_refits$X <- as.character(hddm_refits$X)
names(hddm_refits)[which(names(hddm_refits) == 'X')] <-'sub_id'

#drop hddm columns from test_data
test_data = cbind(test_data$sub_id, test_data[,names(test_data) %in% names(hddm_refits) == FALSE])

#fix naming before merging
names(test_data)[which(names(test_data) == 'test_data$sub_id')] <-'sub_id'

#merge hddm refits to test data
test_data = merge(test_data, hddm_refits, by="sub_id")

#########################
## Intratrial reliability data ####
#########################

# t1_dvs = read.csv(paste0(retest_data_path, 't1_shift_dvs.csv'))
# t2_dvs = read.csv(paste0(retest_data_path, 't2_shift_dvs.csv'))

trial_num_rel_df = read.csv(paste0(retest_data_path, 'trial_num_rel_df_shift.csv'))

#########################
## Demographics data ####
#########################

test_demog <- read.csv(paste0(retest_data_path, '/t1_data/demographic_health.csv'))

retest_demog <- read.csv(paste0(retest_data_path, 'demographic_health.csv'))

retest_demog = retest_demog[retest_demog$X %in% test_demog$X,]

names(test_demog)[which(names(test_demog) == 'X')] <-'sub_id'
names(retest_demog)[which(names(retest_demog) == 'X')] <-'sub_id'

#########################
## Bootstrapped reliability data ####
#########################

### Demographics

demog_boot_df <- read.csv(gzfile(paste0(retest_data_path,'demog_boot_merged.csv.gz')))

demog_boot_df = process_boot_df(demog_boot_df)

### Task and survey measures

boot_df <- read.csv(gzfile(paste0(retest_data_path,'bootstrap_merged.csv.gz')))

boot_df = process_boot_df(boot_df)

boot_df = boot_df[boot_df$dv %in% retest_report_vars,]

# Check if you have all variables bootstrapped
# retest_report_vars[which(retest_report_vars %in% boot_df$dv==FALSE)]

# Boot df contains hddm parameters fit on the full sample in the t1 data
# refits_bootstrap_merged.csv.gz contains bootstrapped reliabilities

refit_boot_df = read.csv(gzfile(paste0(retest_data_path,'refits_bootstrap_merged.csv.gz')))

refit_boot_df = process_boot_df(refit_boot_df)

fullfit_boot_df = boot_df[as.character(boot_df$dv) %in% unique(as.character(refit_boot_df$dv)),]

boot_df = boot_df[!as.character(boot_df$dv) %in% unique(as.character(refit_boot_df$dv)),]

boot_df = rbind(boot_df, refit_boot_df)

rm(refit_boot_df)

boot_df = boot_df %>% mutate(var_subs_pct = var_subs/(var_subs+var_ind+var_resid)*100,
                             var_ind_pct = var_ind/(var_subs+var_ind+var_resid)*100,
                             var_resid_pct = var_resid/(var_subs+var_ind+var_resid)*100)

var_boot_df = boot_df %>%
  group_by(dv) %>%
  summarise(mean_icc = mean(icc),
            mean_pearson = mean(pearson))

rel_comp = lit_review %>%
  left_join(var_boot_df, by = 'dv')

rm(var_boot_df)

#########################
## Duplicate items ####
#########################

tmp = read.csv(gzfile(paste0(retest_data_path, 'items.csv.gz')))
tmp = tmp %>%
  filter(worker == 's005') %>%
  select(item_ID, item_text) %>%
  mutate(item_text = trimws(as.character(item_text))) %>%
  unite(item, c("item_ID", "item_text"), sep = "___")

comb = as.data.frame(t(combn(unique(tmp$item),2)))

duplicate_items = comb %>%
  filter(grepl('dospert', V1)==FALSE) %>%
  filter(grepl('selection_optimization', V1)==FALSE) %>%
  filter(grepl('sensation_seeking', V1)==FALSE) %>%
  separate(V1, c("item1_ID", "item1_text"), sep="___") %>%
  separate(V2, c("item2_ID", "item2_text"), sep="___") %>%
  mutate(similarity = levenshteinSim(item1_text, item2_text)) %>%
  filter(similarity>0.8) %>%
  select(similarity, item1_ID, item2_ID, item1_text, item2_text) %>%
  arrange(-similarity)

duplicate_items_data_t1 = read.csv(paste0(test_data_path, 'subject_x_items.csv'))
duplicate_items_data_t2 = read.csv(paste0(retest_data_path, 'subject_x_items.csv'))

#########################
## Measure labels ####
#########################

measure_labels <- read.csv('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/measure_labels.csv')
measure_labels = measure_labels %>% select(-measure_description)
# Check if there are any missing variables
# retest_report_vars[(retest_report_vars %in% measure_labels$dv == FALSE)]
# measure_labels$dv[(measure_labels$dv %in% retest_report_vars == FALSE)]

#########################
## Meaningful variables ####
#########################

meaningful_vars = read.table('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/meaningful_vars.txt')
meaningful_vars = as.character(meaningful_vars$V1)

#########################
## Bayesian models ####
#########################
# tmp = boot_df %>%
#   select(dv, task, var_subs_pct, var_ind_pct, var_resid_pct)

#summary(lmerTest::lmer(icc ~  task + (1|dv), boot_df))
# m = MCMCglmm(icc ~  task, random = ~dv, data=boot_df, nitt = 1300, burnin = 300)
#icc_by_task_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/icc_by_task_model.rds')

# summary(lmerTest::lmer(var_subs_pct~task+(1|dv),tmp%>%select(-var_ind_pct,-var_resid_pct)))
var_subs_pct_by_task_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/var_subs_pct_by_task_model.rds')

# summary(lmerTest::lmer(var_ind_pct~task+(1|dv),tmp%>%select(-var_subs_pct,-var_resid_pct)))
var_ind_pct_by_task_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/var_ind_pct_by_task_model.rds')

# summary(lmerTest::lmer(var_resid_pct~task+(1|dv),tmp%>%select(-var_subs_pct,-var_ind_pct)))
var_resid_pct_by_task_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/var_resid_pct_by_task_model.rds')

# tmp = measure_labels %>%
#   mutate(dv = as.character(dv)) %>%
#   filter(task == 'task',
#          dv %in% meaningful_vars) %>%
#   left_join(boot_df[,c("dv", "icc")], by = 'dv') %>%
#   separate(dv, c('task_name', 'extra_1', 'extra_2'), sep = '\\.',remove=FALSE) %>%
#   select(-extra_1, -extra_2)

# summary(lm(icc ~ num_all_trials, data = tmp))
# summary(lmerTest::lmer(icc ~ num_all_trials + (1|dv), data = tmp))
icc_by_num_trials_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/icc_by_num_trials_model.rds')

# tmp = measure_labels %>%
#   mutate(dv = as.character(dv),
#          contrast = ifelse(overall_difference == "difference", "contrast", "non-contrast")) %>%
#   filter(ddm_task == 1,
#          rt_acc != 'other') %>%
#   drop_na() %>%
#   left_join(boot_df[,c("dv", "icc")], by = 'dv')

# summary(lmerTest::lmer(icc ~ raw_fit + (1|dv) ,tmp %>% filter(contrast == "non-contrast")))
icc_by_rawfit_noncon_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/icc_by_rawfit_noncon_model.rds')

# summary(lmerTest::lmer(icc ~ raw_fit + (1|dv) ,tmp %>% filter(contrast == "contrast")))
icc_by_rawfit_con_model = readRDS('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/icc_by_rawfit_con_model.rds')
