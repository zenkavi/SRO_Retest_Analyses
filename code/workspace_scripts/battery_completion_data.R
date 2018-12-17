if(from_gh){
  require(RCurl)
}

if(!exists('test_data_path')){
  if(from_gh){
    test_data_path = 'https://raw.githubusercontent.com/zenkavi/Self_Regulation_Ontology/master/Data/Complete_03-29-2018/'
  } else{
    test_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Complete_03-29-2018/' 
  }
}

if(!exists('retest_data_path')){
  if(from_gh){
    retest_data_path = 'https://raw.githubusercontent.com/zenkavi/Self_Regulation_Ontology/master/Data/Retest_03-29-2018/'
  } else{
    retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'
  }
}

#########################
## Battery completion data ####
#########################

workers = read.csv('/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/Local/User_717570_workers.csv')
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

rm(test_comp_date, retest_comp_date)

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