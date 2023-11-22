
#PREDICTED VALUES

## Get predicted values for numeric covariates
median_values <- comb_df %>%
  filter(measure == "crp") %>%
  select(age, ppbmi, tsd, 
         #ifm24hr_allpump_pct,
         date_time_diff_hrs) %>% 
  drop_na() %>%
  summarise_all(median)

## Get middle values for all dummy categorical values, as well as mean value for % pumping
middle_values <- tibble(
  ifm24hr_allpump_pct = mean(survey_clean$ifm24hr_allpump_pct), #0, 
  c19_score = 4,
  income_int = 3,
  edu_int = 2,
  change_yn = 0.5,
  csec_yn = 0.5,
  birthcomps_yn = 0.5,
  nightfeed24hr_yn = 0.5,
  cosleep24hr_yn = 0.5,
  alcohol_yn = 0.5)

## Put them together
covar_values <- cbind(median_values, middle_values)

## Create all unique combinations for each value of % ATN breastfeeding (0-100%)
combinations <- data.frame(ifm24hr_atnbf_pct=seq(0,100,0.1)) %>% 
  tidyr::crossing(covar_values) 

## Gather primary predicted values 
primpred <- map(mod_list, ~ get.predicted(., a = combinations, b = "atn", c = survey_df))

## Put primary predicted values into easy format
primpred_df <- organize.preds(primpred)

## Gather secondary predicted values, by time since delivery and % pumping
tsdpred <- get.predicted(mod_list[["IL-8"]], a = combinations, b = "tsd", c = survey_df)
  #map(mod_list, ~ get.predicted(., a = combinations, b = "tsd", c = survey_df))
pumppred <- get.predicted(mod_list[["CRP"]], a = combinations, b = "pump", c = survey_df)
  #map(mod_list, ~ get.predicted(., a = combinations, b = "pump", c = survey_df))

## Put secondary predicted values into easy format
tsdpred_df <- tsdpred #<- organize.preds(tsdpred)
pumppred_df <- pumppred #<- organize.preds(pumppred)
