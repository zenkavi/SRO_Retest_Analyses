get_var_exp = function(demog_var, dv){
  
  df = merge(test_demog[,c("sub_id", demog_var)], test_data[,c("sub_id", dv)])
  df[,demog_var] = scale(df[,demog_var])
  df[,dv] = scale(df[,dv])
  
  mod = lm(df[,demog_var]~ df[,dv])
  var_exp = summary(mod)$r.squared
  
  return(var_exp)
}

tmp = measure_labels %>% filter(ddm_task == 1 & rt_acc != 'other')
dvs = unique(as.character(tmp$dv))

out = data.frame(dv = dvs)

get_var_exp_for_demog = function(demog_var){
  
  out[,demog_var] = NA
  
  for(i in 1:length(dvs)){
    out[i, demog_var] = get_var_exp(demog_var = demog_var, dv=dvs[i])
  }
  
  return(out)
}

init_demogs = c("HouseholdIncome","RetirementAccount", "HowLongSmoked", "CigsPerDay", "AlcoholHowOften", "CannabisHowOften",  "Nervous", "Hopeless", "RestlessFidgety","Depressed","EverythingIsEffort","Worthless")

results = out

for(i in 1:length(init_demogs)){
  demog_var = init_demogs[i]
  tmp_out = get_var_exp_for_demog(demog_var = demog_var)
  results = merge(results, tmp_out, by="dv")
}

results = merge(results, measure_labels, by="dv")

tmp = results %>%
  select(-ddm_task, -num_all_trials) %>%
  mutate(contrast = ifelse(overall_difference == "difference", 1, 0),
         raw_bin = ifelse(raw_fit == "raw",1,0)) %>%
  gather(key, value, -dv, -overall_difference, -raw_fit, -rt_acc, -contrast, -raw_bin) %>%
  mutate(value = as.numeric(value))
  
#Initial predictions for:

#Desired output: 
  # rows of dvs
  # cols of demog_vars
  # cells of var explained
  
# HeightInches
# WeightPounds
# 
# HouseholdIncome
# RetirementAccount
# RetirementPercentStocks
# RentOwn
# MortgageDebt
# CarDebt
# EducationDebt
# CreditCardDebt
# 
# CoffeeCupsPerDay
# TeaCupsPerDay
# CaffienatedSodaCansPerDay
# CaffieneOtherSourcesDayMG
# 
# GamblingProblem
# TrafficTicketsLastYearCount
# TrafficAccidentsLifeCount
# ArrestedChargedLifeCount
# 
# LifetimeSmoke100Cigs
# HowLongSmoked
# SmokeEveryDay
# CigsPerDay
# HowSoonSmokeAfterWaking
# OtherTobaccoProducts
# 
# AlcoholHowOften
# AlcoholHowManyDrinksDay
# AlcoholHowOften6Drinks
# HowOftenCantStopDrinking
# HowOftenFailedActivitiesDrinking
# HowOftenDrinkMorning
# HowOftenGuiltRemorseDrinking
# HowOftenUnableRememberDrinking
# InjuredDrinking
# RelativeFriendConcernedDrinking
# 
# CannabisPast6Months
# CannabisHowOften
# CannabisHoursStoned
# HowOftenCantStopCannabis
# HowOftenFailedActivitiesCannabis
# HowOftenDevotedTimeCannabis
# HowOftenMemoryConcentrationProblemCannabis
# HowOftenHazardousCannabis
# CannabisConsideredReduction
# 
# Nervous
# Hopeless
# RestlessFidgety
# Depressed
# EverythingIsEffort
# Worthless
# Last30DaysUsual
