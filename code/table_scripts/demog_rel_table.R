library(sjPlot)
library(RCurl)

helper_func_path = 'https://raw.githubusercontent.com/zenkavi/SRO_Retest_Analyses/master/code/helper_functions/'

eval(parse(text = getURL(paste0(helper_func_path,'process_boot_df.R'), ssl.verifypeer = FALSE)))

test_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Complete_03-29-2018/'

retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'

demog_boot_df <- read.csv(gzfile(paste0(retest_data_path,'demog_boot_merged.csv.gz')))

demog_boot_df = process_boot_df(demog_boot_df)

tmp = demog_boot_df %>%
  group_by(dv) %>%
  summarise(median_icc = quantile(icc, probs=0.5),
            icc_2.5 = quantile(icc, probs = 0.025),
            icc_97.5 = quantile(icc, probs = 0.975)) %>%
  arrange(-median_icc)

sjt.df(tmp%>% mutate_if(is.numeric, funs(round(., 3))), describe=F, hide.progress = TRUE, show.rownames = FALSE, file = "/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/output/tables/demog_rel_table.doc")
