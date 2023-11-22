
# MODELS

## Create a data frame with only the variables needed for modeling 
model_df <- comb_df %>% 
  select(delta_value, ifm24hr_atnbf_pct, age, ppbmi, tsd, change_yn, c19_score, 
           ifm24hr_allpump_pct, date_time_diff_hrs, csec_yn, 
           income_int, edu_int, birthcomps_yn, nightfeed24hr_yn, 
           cosleep24hr_yn, alcohol_yn, measure, pid)

## Sanity check
sum(complete.cases(model_df)==TRUE) == nrow(model_df)

## Create list of data frames, split by measure
measure_list <- split(model_df, list(comb_df$measure))

## Apply get.models() 
mod_list <- map(measure_list, get.models)

## Rename model outputs by measure name
names(mod_list) <-  c("CRP", "IL-1ß", "IL-6", "IL-8", "TNF-α")

