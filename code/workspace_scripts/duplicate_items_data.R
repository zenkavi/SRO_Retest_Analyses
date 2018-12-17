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
    retest_data_path = 'https://raw.githubusercontent.com/zenkavi/Self_Regulation_Ontology/retest_scripts/Data/Retest_03-29-2018/'
  } else{
    retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'
  }
}

#########################
## Duplicate items ####
#########################

if(from_gh){
  demog_boot_df = grabRemoteGz(retest_data_path, 'items.csv.gz')
} else{
  tmp = read.csv(gzfile(paste0(retest_data_path, 'items.csv.gz')))
}

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
