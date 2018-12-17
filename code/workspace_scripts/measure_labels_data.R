if(from_gh){
  require(RCurl)
  input_path = 'https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/input/'
}else{
  input_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/'
}

#########################
## Measure labels ####
#########################

measure_labels <- read.csv(paste0(input_path,'measure_labels.csv'))
measure_labels = measure_labels %>% select(-measure_description)
# Check if there are any missing variables
# retest_report_vars[(retest_report_vars %in% measure_labels$dv == FALSE)]
# measure_labels$dv[(measure_labels$dv %in% retest_report_vars == FALSE)]
