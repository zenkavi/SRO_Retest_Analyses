match_t1_t2 <- function(dv_var, t1_df = test_data, t2_df = retest_data, merge_var = 'sub_id', format = "long", sample = 'full', sample_vec){
  
  if(sample == 'full'){
    df = merge(t1_df[,c(merge_var, dv_var)], t2_df[,c(merge_var, dv_var)], by = merge_var) 
  }
  else{
    df = merge(t1_df[t1_df[,merge_var] %in% sample_vec, c(merge_var, dv_var)], t2_df[t2_df[,merge_var] %in% sample_vec, c(merge_var, dv_var)],
               by=merge_var)
  }
  
  df = df %>% 
    na.omit()%>%
    gather(dv, score, -sub_id) %>%
    mutate(time = ifelse(grepl('\\.x', dv), 1, ifelse(grepl('\\.y', dv), 2, NA))) %>%
    separate(dv, c("dv", "drop"), sep='\\.([^.]*)$') %>%
    select(-drop)
  
  
  if(format == 'wide'){
    df = df%>% spread(time, score) 
  }
  
  return(df)
}