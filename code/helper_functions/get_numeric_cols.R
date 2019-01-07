get_numeric_cols = function(df1 = test_data, df2 = retest_data){
  
  numeric_cols = c()
  
  for(i in 1:length(names(df1))){
    
    if(!is.null(df2)){
      if(is.numeric(df1[,i]) & names(df1)[i] %in% names(df2)){
        numeric_cols <- c(numeric_cols, names(df1)[i])
      }
    } else{
      if(is.numeric(df1[,i])){
        numeric_cols <- c(numeric_cols, names(df1)[i])
      }
    }
  }
  
  return(numeric_cols)
}

