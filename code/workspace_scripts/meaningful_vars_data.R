if(from_gh){
  require(RCurl)
  input_path = 'https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/input/'
}else{
  input_path = '/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/'
}

#########################
## Meaningful variables ####
#########################

meaningful_vars = read.table(paste0(input_path,'meaningful_vars.txt'))
meaningful_vars = as.character(meaningful_vars$V1)
