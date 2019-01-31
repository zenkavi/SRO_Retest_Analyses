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
         task_group = str_to_title(task_group),
         task_group = gsub(" $","", task_group, perl=T)) %>%
  mutate(task_group = ifelse(task_group == "Psychological Refractory Period Two Choices", "PRP", ifelse(task_group == "Angling Risk  Always Sunny", "Angling Risk", ifelse(task_group == "Two Stage", "Two Step", ifelse(task_group == "Threebytwo", "Task Switching", ifelse(task_group == "Adaptive N Back", "Adaptive N-back", ifelse(task_group == "Go Nogo", "Go/No-go",  ifelse(task_group == "Ravens", "Raven's", ifelse(task_group == "Columbia Card  Hot", "CCT Hot", ifelse(task_group == "Columbia Card  Cold", "CCT Cold", ifelse(task_group == "Probabilistic Selection", "Prob Selection" ,ifelse(task_group == "Choice Reaction Time", "Choice RT", ifelse(task_group == "Simple Reaction Time", "Simple RT",ifelse(task_group == "Local Global Letter", "Local Global" ,ifelse(task_group == "Attention Network", "ANT",ifelse(task_group == "Dot Pattern Expectancy", "DPX",ifelse(task_group == "Motor Selective Stop Signal", "Motor SSS",ifelse(task_group == "Stim Selective Stop Signal", "Stim SSS",ifelse(task_group == "Cognitive Reflection", "CRT",ifelse(task_group == "Discount Titrate", "Discounting", ifelse(task_group == "Two Stage Decision", "Two Step",ifelse(task_group == "Dietary Decision", "Dietary", task_group)))))))))))))))))))))) %>%
  mutate(task_group = ifelse(task_group == "Bis Bas", "BIS-BAS", ifelse(task_group == "Bis11", "BIS-11", ifelse(task_group == "Dospert Eb", "DOSPERT EB", ifelse(task_group == "Dospert Rp", "DOSPERT RP", ifelse(task_group == "Dospert Rt", "DOSPERT RT", ifelse(task_group == "Erq", "ERQ", ifelse(task_group == "Upps Impulsivity", "UPPS-P", ifelse(task_group == "Mindful Attention Awareness", "MAAS",ifelse(task_group == "Mpq Control", "MPQ Control",ifelse(task_group == "Ten Item Personality", "TIPI",ifelse(task_group == "Impulsive Venture", "I-7",ifelse(task_group == "Leisure Time Activity", "L-CAT",ifelse(task_group == "Selection Optimization Compensation", "SOC",ifelse(task_group == "Future Time Perspective", "FTP",task_group))))))))))))))) %>%
  select(-measure_description)
