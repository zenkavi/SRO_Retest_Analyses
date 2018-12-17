process_boot_df = function(df){
  df = df %>%
    drop_na() %>%
    mutate(dv = as.character(dv),
           icc2.1 = as.numeric(as.character(icc2.1)),
           icc3.k = as.numeric(as.character(icc3.k)),
           spearman = as.numeric(as.character(spearman)),
           pearson = as.numeric(as.character(pearson)),
           sem = as.numeric(as.character(sem)),
           partial_eta = as.numeric(as.character(partial_eta)),
           var_subs = as.numeric(as.character(var_subs)),
           var_ind = as.numeric(as.character(var_ind)),
           var_resid = as.numeric(as.character(var_resid)))
  return(df)} 