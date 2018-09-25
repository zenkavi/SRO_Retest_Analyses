library(tidyverse)

remove_correlated_task_variables = function(data, threshold=0.85){
  tasks = strsplit(names(data), "\\.")
  tasks = unique(unlist(lapply(tasks, `[[`, 1)))
  tasks = tasks[-which(tasks=="sub_id")]
  
  columns_to_remove = c()
  
  for(task in tasks){
    task_data = data[,grep(task, names(data), value=T)]
   
    corr_mat = data.frame(cor(task_data, use="pairwise.complete.obs"))
    corr_mat = corr_mat %>%
      mutate(dv = row.names(.)) %>%
      gather(key, value, -dv) %>%
      filter(value != 1 & duplicated(value)==FALSE)
    
    task_columns_to_remove = corr_mat %>%
      filter(value > 0.85)
    task_columns_to_remove = task_columns_to_remove$dv
    
    columns_to_remove = c(columns_to_remove, task_columns_to_remove)
  }
  
  cat(rep('*', 40))
  cat('\n')
  cat(paste0('Dropping ', length(columns_to_remove) ,' variables with correlations above ', threshold))
  cat('\n')
  cat(columns_to_remove, sep = '\n')
  cat(rep('*', 40))
  cat('\n')

  data = data %>% select(-one_of(columns_to_remove))
  
  return(data)
}

