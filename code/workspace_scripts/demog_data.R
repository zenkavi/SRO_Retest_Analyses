#########################
## Demographics data ####
#########################

test_demog <- read.csv(paste0(retest_data_path, '/t1_data/demographic_health.csv'))

retest_demog <- read.csv(paste0(retest_data_path, 'demographic_health.csv'))

retest_demog = retest_demog[retest_demog$X %in% test_demog$X,]

names(test_demog)[which(names(test_demog) == 'X')] <-'sub_id'
names(retest_demog)[which(names(retest_demog) == 'X')] <-'sub_id'