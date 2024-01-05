#IMPORT DATA

bio_df <- read.csv("./data/biomarker_data.csv") %>% 
  select(-X) 
# %>% 
#   filter((measure == "crp" & value <= 1600 & value >= 25) | measure != "crp")

survey_df <- read.csv("./data/survey_data.csv") %>% select(-X) %>%
  mutate(cosleep24hr_yn = case_when(grepl("My baby slept in bed with me", cosleep24hr) ~ 1,
                                    TRUE ~ 0),
         birthcomps_yn = case_when(str_length(birthcomps)>2 ~ 1,
                                   is.na(birthcomps) ~ 0),
         nightfeed24hr_yn = case_when(nightfeed24hr_yn == "Yes" ~ 1,
                                      nightfeed24hr_yn == "No" ~ 0),
         alcohol_yn = case_when(alcohol_yn == "Yes" ~ 1,
                                alcohol_yn == "No" ~ 0),
         csec_yn = case_when(grepl("Cesarean|Other",deliverymethod) ~ 1,
                             TRUE ~ 0),
         income_int = case_when(income == "$20,000 - $39,999" ~ 1,
                            income == "$40,000 - $59,999" ~ 2,
                            income == "$60,000 - $79,999" ~ 3,
                            income == "$80,000 - $99,999" ~ 4,
                            income == "More than $100,000" ~ 5),
         edu_int = case_when(edu == "College degree" ~ 2,
                         edu == "High school graduate" ~ 1,
                         edu == "Professional degree" ~ 3),
         vax_revised = case_when(is.na(vax_yn) ~ "Unknown", TRUE ~ vax_yn),
         #Create binary variable indicating whether or not participant was asked the "future" pumping question
         change_yn = case_when(is.na(ifm24hr_futurepumped) ~ 0, TRUE ~ 1)) %>%
  group_by(pid) %>%
  #All pumping, 24 hrs and 2 wks
  mutate(ifm24hr_allpump = sum(ifm24hr_storedpumped, ifm24hr_pumped, ifm24hr_futurepumped, na.rm = T),
         ifm24hr_allpump_pct = ifm24hr_allpump/ifm24hr_total*100,
         ifm2wks_allpump = sum(ifm2wks_storedpumped, ifm2wks_pumped, na.rm = T)) %>%
  ungroup(pid)

#Combine data from saliva and survey 
comb_df <- bio_df %>%
  select(pid, measure, sample, secretion_rate, unit, date_time_diff_hrs) %>%
  pivot_wider(values_from = secretion_rate, names_from = sample) %>%
  mutate(delta_value = `2` - `1`) %>% #Morning - evening
  full_join(., survey_df) %>%
  mutate(saliva_yn = case_when(is.na(delta_value) ~ "No", TRUE ~ "Yes")) %>%
  filter(saliva_yn == "Yes", measure %in% c("crp","il6","il8","il1b","tnfa"))

survey_clean <- survey_df %>% filter(pid %in% unique(comb_df$pid)) %>%
  mutate(ethnicity_grouped = case_when(grepl("\\,",ethnicity) ~ "Multi-racial/multi-ethnic",
                                       TRUE ~ ethnicity))
