if(!exists('test_data_path')){
  test_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Complete_03-29-2018/'
}

if(!exists('retest_data_path')){
  retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'
}

if(!exists('lit_review')){
  source('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/code/workspace_scripts/lit_review_data.R')
}

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