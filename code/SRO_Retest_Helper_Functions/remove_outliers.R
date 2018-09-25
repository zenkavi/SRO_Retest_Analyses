remove_outliers = function(data_column, quantile_range = 2.5){
  
  q_25 = quantile(data_column, na.rm=T)[2]
  q_50 = quantile(data_column, na.rm=T)[3]
  q_75 = quantile(data_column, na.rm=T)[4]
  
  lowlimit = q_50 - quantile_range*(q_75 - q_25)
  highlimit = q_50 + quantile_range*(q_75 - q_25)
  
  data_column = ifelse(data_column<lowlimit, NA, ifelse(data_column>highlimit, NA, data_column))
  
  return(data_column)
}