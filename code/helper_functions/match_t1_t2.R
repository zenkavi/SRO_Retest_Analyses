require(tidyverse)

match_t1_t2 <- function(dv_var, t1_df, t2_df, merge_var = 'sub_id', format = "long", sample = 'full'){
  
  if(sample == 'full'){
    df = merge(t1_df[,c(merge_var, dv_var)], t2_df[,c(merge_var, dv_var)], by = merge_var) 
  } else{
    df = cbind(t1_df[,c(merge_var, dv_var)], t2_df[,c(dv_var)])
    names(df) = c(merge_var, paste0(dv_var,'.x'), paste0(dv_var, '.y'))
  }
  
  df = df %>% 
    na.omit()%>%
    gather(dv, score, -sub_id) %>%
    mutate(time = ifelse(grepl('\\.x', dv), 1, ifelse(grepl('\\.y', dv), 2, NA))) %>%
    separate(dv, c("dv", "drop"), sep='\\.([^.]*)$') %>%
    select(-drop)
  
  
  if(format == 'wide'){
    if(sample == 'full'){
      df = df%>% spread(time, score) 
    } else{
      a = df[1:(nrow(df)/2),]
      b = df[((nrow(df)/2)+1):nrow(df),]
      df = cbind(a%>%select(-time), b %>%select(score))
      names(df) = c(merge_var, "dv", 1, 2)
    }
    
  }
  
  return(df)
}