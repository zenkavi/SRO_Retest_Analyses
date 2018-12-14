#########################
## Literature review data ####
#########################

if(from_gh){
  require(RCurl)
  lit_review <- read.csv('https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/input/lit_review_figure.csv')
}else {
  lit_review <- read.csv('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/lit_review_figure.csv')
  }

require(tidyverse)
require(stringr)
lit_review = lit_review %>%
  separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
  mutate(task_group = factor(task_group, levels = unique(task_group[order(task)])),
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
