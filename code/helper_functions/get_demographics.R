get_demographics = function(dataset=NA, cleanup=TRUE, num_response_thresh=10, drop_categorical=TRUE){
  require(tidyverse)
  
  data = read.csv(dataset)
  
  if(cleanup){
    data = data %>%
      mutate(WeightPounds = ifelse(WeightPounds<50, NA, WeightPounds),
             HeightInches = ifelse(HeightInches<36, NA, HeightInches),
             CaffienatedSodaCansPerDay = ifelse(CaffienatedSodaCansPerDay < 0, NA, CaffienatedSodaCansPerDay),
             CaffieneOtherSourcesDayMG = ifelse(CaffieneOtherSourcesDayMG>2000, NA, CaffieneOtherSourcesDayMG),
             BMI = WeightPounds*0.45/(HeightInches*0.025)^2,
             Obese = ifelse(BMI>30, 1, 0)) %>%
      select(-WeightPounds, -HeightInches)
  }
  
  categorical_vars = c('HispanicLatino','Race',
                       'DiseaseDiagnoses', 'DiseaseDiagnosesOther',
                       'MotivationForParticipation', 'MotivationOther',
                       'NeurologicalDiagnoses',
                       'NeurologicalDiagnosesDescribe',
                       'OtherDebtSources',
                       'OtherDrugs', 'OtherRace', 'OtherTobaccoProducts',
                       'PsychDiagnoses',
                       'PsychDiagnosesOther')
  
  if(drop_categorical){
    data = data %>%
      select(-one_of(categorical_vars))
  }
  
  if(num_response_thresh>0){
    data = data %>%
      select(which(colMeans(is.na(.)) < num_response_thresh/nrow(.)))
  }
  
  return(data)
}