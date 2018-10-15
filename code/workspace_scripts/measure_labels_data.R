#########################
## Measure labels ####
#########################

measure_labels <- read.csv('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/measure_labels.csv')
measure_labels = measure_labels %>% select(-measure_description)
# Check if there are any missing variables
# retest_report_vars[(retest_report_vars %in% measure_labels$dv == FALSE)]
# measure_labels$dv[(measure_labels$dv %in% retest_report_vars == FALSE)]
