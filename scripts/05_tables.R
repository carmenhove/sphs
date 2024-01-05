
## TABLES 

# MAIN MANUSCRIPT TABLES

## Create look-up table
rename_tbl <- c(
  phq_cold = "Maternal Minor Cold", 
  phq_gastro = "Maternal Gastrointestinal Illness",
  phq_respiratory = "Maternal Severe Respiratory Illness", 
  la_yn = "Lactational Amenorrhea",
  csec_yn = "C-section", 
  birthcomps_yn = "Birth Complication(s)",
  cosleep24hr_yn = "Co-sleeping (24 hr)", 
  nightfeed24hr_yn = "Night Feeding (24 hr)", 
  alcohol_yn = "Alcohol (24 hr)",
  age = "Age", 
  tsd = "Days Since Delivery", 
  ppbmi = "Pre-pregnancy BMI", 
  cbmi = "Current BMI", 
  ifm24hr_total = "Total Infant Feedings (24 hr)",
  date_time_diff_hrs = "Hours Between Samples (24 hr)",
  ifm24hr_atnbf_pct = "% ATN Breastfeeding (24 hr)",
  ifm24hr_allpump_pct = "% Pumping (24 hr)",
  parity = "Parity",
  everbf = "Ever ATN breastfed",
  everpump = "Ever pumped",
  mixed_24hr = "Mixed feeding (24 hr)",
  ebf_24hr = "Exclusive ATN breastfeeding (24 hr)",
  everliquids = "Ever used non-breastmilk liquids" ,
  eversolids = "Ever used solid food",
  nobf_24hr = "No ATN breastfeeding (24 hr)",
  onlybf = "Exclusive ATN breastfeeding",
  everdonated = "Ever donated"
)

## Table 1
tbl_1 <- data.frame(Measure = c("CRP", "IL-1ß", "IL-6","IL-8","TNF-α"),
                 Description = c("Acute phase protein synthesized by the liver in response to cytokine stimulation during an inflammatory event; a non-specific measure of systemic inflammation.", "Pyrogenic cytokine secreted by monocytes and macrophages. Activated by exposure to essentially all microbial products via TLR ligands. Mediates inflammatory response and immune cell activity.", "Secreted by macrophages, osteoclasts, and smooth muscle cells. Inhibits effects of TNF-α. Mediator of acute phase response. Stimulates acute phase protein synthesis, production of neutrophils, and B cell growth. Inhibits regulatory T cells.","Chemokine produced by macrophages and epithelial cells. Exerts strong specificity for neutrophils, weak effects on other leukocytes. Stimulates phagocytosis by recruited immune cells.","Both a pyrogenic cytokine and an adipokine. Promotes insulin resistance. Produced by macrophages. Escalates inflammatory response."))

str(tbl_1)

## Table 2
tbl_2 <- comb_df %>%
  #limit to CRP, since this is what we did to standardize model predicted values
  filter(measure == "crp") %>%
  select(age, tsd, ifm24hr_total, date_time_diff_hrs, ifm24hr_atnbf_pct,
         ifm24hr_allpump_pct, ppbmi, cbmi, parity) %>%
  summarise(across(everything(), 
                   list(Median = median, IQR = IQR, 
                        Mean = mean, `Standard Deviation` = sd, Min = min, Max = max), na.rm = TRUE)) %>% 
  #summarise(across(everything(), get.meanmed.range)) %>%
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value") %>% 
  mutate(Value = round(Value, digits = 2)) %>% 
  separate(Statistic, c("Variable","Statistic"), sep = "\\_(?=[^_]+$)") %>% 
  pivot_wider(names_from = Statistic, values_from = Value) 

tbl_2$Variable <- unname(rename_tbl[tbl_2$Variable])

#   <!-- 5. In table 3, many of the variables require further definition. For example, what does 'gastro' mean?   Is this table referring to incidence rate in the lactating parent or infant?  Use complete terminology, not jargon. Use footnotes to explain brief terms, such as using footnotes to list the birth complications included in the incidence rate. -->

## Table S4
tbl_3 <- survey_clean %>%
  select(pid, phq_cold, phq_gastro, phq_respiratory, la_yn, csec_yn, birthcomps_yn,
         cosleep24hr_yn, nightfeed24hr_yn, alcohol_yn) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(phq_cold:alcohol_yn, values_to = "value", names_to = "Variable") %>%
  mutate(value = case_when(value == "0" ~ "No", value == "1" ~ "Yes", TRUE ~ value)) %>%
  group_by(Variable, value) %>%
  tally() %>%
  pivot_wider(names_from = value, values_from = n) %>%
  mutate(Incidence = str_c(round(Yes/(No+Yes)*100,digits = 2),"%")) %>%
  arrange(Yes) %>%
  select(Variable, Yes, No, Incidence) %>%
  ungroup(Variable) 

tbl_3$Variable <- unname(rename_tbl[tbl_3$Variable])

## Table 4
tbl_4 <- survey_clean %>%
  select(c19, pid) %>%
  filter(!is.na(c19)) %>%
  separate(c19, c("1","2","3","4","5","6","7","8"), sep = ",") %>%
  group_by(pid) %>%
  pivot_longer(`1`:`8`) %>%
  filter(!is.na(value)) %>%
  select(-name) %>%
  group_by(value) %>%
  tally() %>%
  mutate(percent = str_c(format(round(n/96*100, digits =2),2),"%")) %>%
  filter(value != "Change in breastfeeding goals") %>%
  arrange(n) %>%
  rename(`COVID-19 effect` = value, N = n, Percent = percent)

#Table 5
tbl_5 <- unique(
  primpred_df %>%
    filter(measure %in% c("CRP","IL-8")) %>%
    group_by(measure) %>%
    slice(which.max(Estimate),
          which.min(Estimate),
          which.max(ifm24hr_atnbf_pct),
          which.min(ifm24hr_atnbf_pct))
) %>%
  mutate(ifm24hr_atnbf_pct = round(ifm24hr_atnbf_pct,0),
         value = str_c("Median: ", 
                       format(round(Estimate,2),2),
                       " pg/ml/min; 95% CI: ",
                       format(round(Q5,2),2),
                       ", ", 
                       format(round(Q95,2),2))) %>%
  arrange(measure, ifm24hr_atnbf_pct) %>%
  rename(Measure = measure,
         `Predicted Value` = value) %>%
  mutate(`% ATN Breastfeeding` = str_c(ifm24hr_atnbf_pct, "%")) %>%
  select(Measure, `% ATN Breastfeeding`, `Predicted Value`) 
  

#-------------------------------------------------------------------------------

# SUPPLEMENTAL TABLES

# <!-- 8. Report more details on the cytokine and CRP assays. What was the limit of detection for these assays? How often did values exceed the range of the standard curve for each assay, and how were these data handled? -->

## Table S1

# Samples were assayed for the Salimetrics Cytokine Panel (IL-1β, IL-6, TNF-α, and IL-8) in duplicate at the Salimetrics SalivaLab (Carlsbad, CA) using a proprietary electrochemiluminesence method developed and validated for saliva by Salimetrics. The average coefficient of variation for all samples tested was <15%, which meets the SalivaLab’s criteria for accuracy and repeatability in Salivary Bioscience, and exceeds the applicable NIH guidelines for Enhancing Reproducibility through Rigor and Transparency.

tbl_s1 <- as_tibble(data.frame(Measure = c("CRP",
                                           "IL-6","IL-1ß",
                                           "IL-8","TNF-α"),
                               Sensitivity = c("0.042 pg/mL", 
                                               "0.0491 pg/mL", "0.0195 pg/mL",
                                               "0.0201 pg/mL", "0.0314 pg/mL"),
                               Range = c("25-1600 pg/mL", 
                                         "0.0491-736 pg/mL", "0.0195-589 pg/mL",
                                         "0.0201-574 pg/mL", "0.0314-380 pg/mL"),
                               CV = c("2.61%","5.00%","2.51%","3.80%","6.46%"))) %>%
  rename(`Coefficient of Variability` = CV)

get.breakdown <- function(x, b){
  tbl <- x %>%
    group_by(.data[[b]]) %>%
    tally() %>%
    mutate(percent = str_c(format(round(n/96*100, 2),2),"%")) %>%
    arrange(n) %>%
    rename(N = n, Percent = percent)
  tbl
}

## Table S2
tbl_s2 <- get.breakdown(survey_clean, "income") %>%
  rename(Income = income)

## Table S3
tbl_s3 <- get.breakdown(survey_clean, "edu") %>%
  rename(Education = edu)

## Table S4
tbl_s4 <- get.breakdown(survey_clean, "relstatus") %>%
  rename(`Relationship Status` = relstatus)

## Table S5
tbl_s5 <- get.breakdown(
  survey_clean %>% filter(!is.na(ethnicity_grouped)), 
  "ethnicity_grouped") %>%
  rename(`Race/Ethnicity` = ethnicity_grouped)

## Table S6
tbl_s6 <- get.breakdown(survey_clean, "employ") %>%
  rename(`Employment Status` = employ)

## Table S7
tbl_s7 <- comb_df %>%
  group_by(measure) %>% 
  summarise(across(`1`:`2`, list(Median = median,  IQR = IQR, 
                                 Mean = mean, `Standard Deviation` = sd,
                                 Min = min, Max = max), na.rm = T)) %>% 
  pivot_longer(`1_Median`:`2_Max`, values_to = "Value", names_to = "Statistic") %>%
  separate(Statistic, c("Sample", "Statistic"), sep = "\\_(?=[^_]+$)") %>% 
  mutate(Value = round(Value, digits = 2)) %>% 
  pivot_wider(names_from = Statistic, values_from = Value) %>% 
  mutate(Measure = case_when(measure == "crp" ~ "CRP",
                             measure == "il1b" ~ "IL-1ß",
                             measure == "il6" ~ "IL-6",
                             measure == "il8" ~ "IL-8",
                             measure == "tnfa" ~ "TNF-α")) %>% 
  select(Measure, Sample:Max)

## Table S8
tbl_s8 <- survey_clean %>%
  mutate(mixed_24hr = case_when(ifm24hr_atnbf > 0 & 
                                  (ifm24hr_liquids > 0 | 
                                     ifm24hr_solids > 0 | 
                                     ifm24hr_pumped > 0 | 
                                     ifm24hr_storedpumped > 0 | 
                                     ifm24hr_donated > 0 | 
                                     ifm24hr_futurepumped > 0) ~ 1,
                                TRUE ~ 0),
         ebf_24hr = case_when(ifm24hr_atnbf_pct == 100 & 
                                ifm24hr_liquids == 0 &
                                ifm24hr_solids == 0 &
                                ifm24hr_pumped == 0 &
                                ifm24hr_storedpumped == 0 &
                                ifm24hr_donated == 0 &
                                (is.na(ifm24hr_futurepumped) |
                                   ifm24hr_futurepumped == 0) ~ 1,
                              TRUE ~ 0),
         nobf_24hr = case_when(ifm24hr_atnbf == 0 ~ 1, TRUE ~ 0),
         everbf = case_when(everbf_yn == "No" ~ 0, TRUE ~ 1),
         onlybf = case_when(ebf_24hr == 1 & ifm2wks_atnbf == 100 & 
                              ifmintro == "No, I haven't used any of these methods" ~ 1,
                            TRUE ~0),
         everpump = case_when(ifm24hr_pumped > 0 |
                                ifm24hr_storedpumped > 0 |
                                ifm24hr_futurepumped > 0 |
                                ifm2wks_pumped > 0 | 
                                ifm2wks_storedpumped > 0 |
                                !is.na(ifmintro_pumped) ~ 1,
                              TRUE ~ 0),
         eversolids = case_when(ifm24hr_solids> 0 | ifm2wks_solids > 0| 
                                  !is.na(ifmintro_solids) ~ 1,
                                TRUE ~ 0),
         everliquids = case_when(ifm24hr_liquids> 0 | ifm2wks_liquids > 0| 
                                   !is.na(ifmintro_liquids) ~ 1,
                                 TRUE ~ 0),
         everdonated = case_when(ifm24hr_donated > 0 | ifm2wks_donated > 0| 
                                   !is.na(ifmintro_donated) ~ 1,
                                 TRUE ~ 0)) %>%
  select(pid, mixed_24hr, ebf_24hr, nobf_24hr, everbf, onlybf, 
         everpump, eversolids, everliquids, everdonated) %>%
  pivot_longer(mixed_24hr:everdonated, 
               names_to = "Covariate",
               values_to = "Value") %>%
  group_by(Covariate, Value) %>%
  tally() %>%
  pivot_wider(values_from = n, 
              names_from = Value) %>%
  rename(No = `0`, Yes = `1`) %>%
  mutate(No = case_when(is.na(No) ~ 0, TRUE ~ No),
         Incidence = str_c(format(round(Yes/(Yes+No)*100,2),2),"%")) %>%
  arrange(Yes) %>%
  select(Covariate, Yes, No, Incidence)

tbl_s8$Covariate <- unname(rename_tbl[tbl_s8$Covariate])

#Table S8
fits_list <- map(mod_list, get.fits)
names(fits_list) <- names(mod_list)

tbl_s9 <- bind_rows(fits_list, .id = "measure") %>%
  arrange(parameter, measure) %>%
  filter(parameter %in% c("tsd","ifm24hr_allpump_pct", "ifm24hr_atnbf_pct")) %>%
  select(-sig) %>%
  mutate(parameter = case_when(parameter == "ifm24hr_atnbf_pct" ~ "% ATN breastfeeding",
                               parameter == "ifm24hr_allpump_pct" ~ "% pumping",
                               TRUE ~ "Days since delivery")) %>%
  rename(Measure = measure,
         Parameter = parameter,
         Coefficient = param_adj) %>%
  arrange(Parameter, Measure)

