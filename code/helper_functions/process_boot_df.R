process_boot_df = function(df){
  df = df %>%
    drop_na() %>%
    mutate(dv = as.character(dv),
           icc = as.numeric(as.character(icc)),
           spearman = as.numeric(as.character(spearman)),
           pearson = as.numeric(as.character(pearson)),
           eta_sq = as.numeric(as.character(eta_sq)),
           sem = as.numeric(as.character(sem)),
           partial_eta_sq = as.numeric(as.character(partial_eta_sq)),
           omega_sq = as.numeric(as.character(omega_sq)),
           var_subs = as.numeric(as.character(var_subs)),
           var_ind = as.numeric(as.character(var_ind)),
           var_resid = as.numeric(as.character(var_resid)),
           F_time = as.numeric(as.character(F_time)),
           p_time = as.numeric(as.character(p_time)),
           df_time = as.numeric(as.character(df_time)),
           df_resid = as.numeric(as.character(df_resid)))
  return(df)} 