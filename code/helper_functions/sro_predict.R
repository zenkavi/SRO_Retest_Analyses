sro_predict = function(x_df, y_df, cv_folds = 10){
  
  require(tidyverse)
  
  out = data.frame(dv=NA, iv=NA, Rsquared=NA, RsquaredSD=NA)
  
  if("sub_id" %in% names(x_df)){
    x_s = names(x_df)[-which(names(x_df)=="sub_id")]
  }
  else{
    x_s = names(x_df)
  }
  if("sub_id" %in% names(y_df)){
    y_s = names(y_df)[-which(names(y_df)=="sub_id")]
  }
  else{
    y_s = names(y_df)
    
  }
  
  for(i in y_s){
    for(j in x_s){
      
      x = x_df%>%select(j)
      y = y_df[,i]
      
      print(paste0('Running CV for y= ', i, ' and x= ', j))
      
      model = train(x,y,
                    method="lm",
                    trControl = trainControl(method="cv", number=cv_folds),
                    na.action = na.exclude)
      
      tmp = data.frame(dv = i, iv = j, Rsquared = model$results$Rsquared, RsquaredSD = model$results$RsquaredSD)
      
      out = rbind(out, tmp)
      
      print("Done with loop. Saving...")
    }
  }
  
  out = out[-1,]
  return(out)
}