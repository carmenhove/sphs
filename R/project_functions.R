#PROJECT FUNCTIONS

## function to calculate median and range
get.medrange <- function(x){
  val <- str_c(format(round(median(x, na.rm = T),2), 2), " (",
               format(round(min(x, na.rm = T),2),2), " - ",
               format(round(max(x, na.rm = T),2),2), ")")
  val
}

## function to calculate mean and range
get.meanrange <- function(x){
  val <- str_c(format(round(mean(x, na.rm = T),2),2), " (",
               format(round(min(x, na.rm = T),2),2), " - ",
               format(round(max(x, na.rm = T),2),2), ")")
  val
}

## function to calculate mean and median, plus range
get.meanmed.range <- function(x){
  val <- str_c(format(round(median(x, na.rm = T),2),2), " / ",
               format(round(mean(x, na.rm = T),2),2), " (",
               format(round(min(x, na.rm = T), 2),2), " - ",
               format(round(max(x, na.rm = T), 2),2), ")")
  val
}

## function to run models
get.models <- function(x){

  if(grepl("crp|il8", unique(x$measure))) {
    set.seed(1234)
    prior <- get_prior(delta_value ~ s(ifm24hr_atnbf_pct) + age + ppbmi + tsd + 
                         change_yn + c19_score + ifm24hr_allpump_pct + 
                         date_time_diff_hrs +csec_yn + 
                         income_int + edu_int + birthcomps_yn + 
                         nightfeed24hr_yn + cosleep24hr_yn + alcohol_yn,
                       data = x, family = gaussian())
    mod <- brm(delta_value ~ s(ifm24hr_atnbf_pct) + age + ppbmi + tsd + change_yn + c19_score + 
                 ifm24hr_allpump_pct + date_time_diff_hrs +csec_yn + 
                 income_int + edu_int + birthcomps_yn + nightfeed24hr_yn + 
                 cosleep24hr_yn + alcohol_yn,
               data = x, family = gaussian(),
               prior = prior,
               control = list(adapt_delta = 0.9999),
               iter = 25000, chains=2, cores=2)
  } else {
    set.seed(1234)
    prior <- get_prior(delta_value ~ ifm24hr_atnbf_pct + age + ppbmi + tsd + 
                         change_yn + c19_score + ifm24hr_allpump_pct + 
                         date_time_diff_hrs +csec_yn + 
                         income_int + edu_int + birthcomps_yn + 
                         nightfeed24hr_yn + cosleep24hr_yn + alcohol_yn,
                       data = x, family = gaussian())
    mod <-brm(delta_value ~ ifm24hr_atnbf_pct + age + ppbmi + tsd + change_yn + c19_score + 
                ifm24hr_allpump_pct + date_time_diff_hrs +csec_yn + 
                income_int + edu_int + birthcomps_yn + nightfeed24hr_yn + 
                cosleep24hr_yn + alcohol_yn,
              prior = prior,
              data = x, family = gaussian(),
              iter = 10000, chains=2, cores=2)
  }
  mod
}

get.predicted <- function(x, a, b, c){ #a = combinations, b = measure of interest
  
  if(b == "tsd") {
    tsd_min <- a %>% mutate(tsd = min(c$tsd)) 
    tsd_mid <- a %>% mutate(tsd = max(c$tsd)/2)
    tsd_max <- a %>% mutate(tsd = max(c$tsd)) 
    
    a <- full_join(tsd_min, tsd_mid) %>% 
      full_join(., tsd_max) 
    
  } else if(b == "pump"){
    pump_zero <- a %>% mutate(ifm24hr_allpump_pct= 0)
    pump_50 <- a %>% mutate(ifm24hr_allpump_pct= 50)
    pump_100 <- a %>% mutate(ifm24hr_allpump_pct= 100)
    
    a <- full_join(pump_zero, pump_50) %>%
      full_join(., pump_100) 
    
  } 
  
  set.seed(1234)
  pred <- as_tibble(fitted(x, 
                           newdata = a,
                           re_formula = NA, summary = T,
                           robust = TRUE,
                           probs = c(0.05, 0.95))) 
  
  if(b == "tsd"){
    pred_final <- pred %>% 
      mutate(ifm24hr_atnbf_pct = a$ifm24hr_atnbf_pct,
             tsd = a$tsd,
             measure = "IL-8",
             tsd = str_c(tsd, " days \n since delivery"),
             tsd = ordered(tsd, levels = c("8 days \n since delivery",
                                           "106 days \n since delivery",
                                           "212 days \n since delivery")))
  } else if(b == "pump"){
    pred_final <- pred %>%
      mutate(ifm24hr_atnbf_pct = a$ifm24hr_atnbf_pct,
             ifm24hr_pump_pct = a$ifm24hr_allpump_pct,
             measure = "CRP",
             pump = str_c(ifm24hr_pump_pct, "% Pumping"),
             pump = ordered(pump, levels = c("0% Pumping",
                                             "50% Pumping",
                                             "100% Pumping")))
  } else if(b == "atn"){
    pred_final <- pred %>%
      mutate(ifm24hr_atnbf_pct = a$ifm24hr_atnbf_pct)
  }
  
  pred_final
  
}

## function to organize predicted values
organize.preds <- function(x){
  x2 <- bind_rows(x, .id = "measure") %>%
    mutate(measure = ordered(
      measure, levels = c("CRP", "IL-8", "IL-6", "IL-1ß", "TNF-α")))
  x2
}

## function to generate plots
get.plot <- function(a,b){
  plot <- ggplot(a) +
    geom_ribbon(aes(x = b, y = Estimate, fill = measure,
                    ymin = Q5, ymax = Q95),
                alpha = 0.5, colour = NA) + 
    geom_line(aes(x = b, y = Estimate, color = measure)) + 
    geom_hline(yintercept = 0, color = "black", alpha = 0.3, linetype = "longdash") + 
    facet_wrap(measure ~ ., scales = "free", ncol = 5)+
    guides(fill="none", color = "none") +
    theme(plot.caption = element_text(hjust = 0),
          plot.title.position = "plot", 
          plot.caption.position =  "plot")#+
   #theme(text = element_text(size = 7),
  #      plot.title = element_text(size=11))
  plot
}

## Extract model coefficients
get.fits <- function(x){
  fits <- as_tibble(fixef(x, probs = c(0.05, 0.95))) %>%
    mutate(parameter = dimnames(fixef(x, probs = c(0.05, 0.95)))[[1]],
           unit = case_when(grepl("_pct",parameter) ~ "pg/ml/min/%",
                            parameter == "tsd" ~ "pg/ml/min/day",
                            TRUE ~ ""),
           param_adj = str_c("ß = ", format(round(Estimate, digits = 2),2), " ", unit, 
                             "; 95% CI = ", format(round(Q5, digits = 2),2), ", ", 
                             format(round(Q95, digits = 2),2)),
           sig = case_when(Q5 > 0 & Q95 > 0 ~ "Positive",
                           Q5 < 0 & Q95 < 0 ~ "Negative",
                           TRUE ~ "Non-Robust")) %>%
    select(parameter, param_adj, sig) %>%
    arrange(sig)
    
  fits
}

## get linear coefficients for % ATN breastfeeding
get.coef <- function(x){
  tbl <- get.fits(x) %>% 
    filter(parameter == "ifm24hr_atnbf_pct")
  value <- tbl$param_adj
  value
}

## get variance parameters for all non-linear covariates from models
get.splinefit <- function(x){
  x1 <- summary(x)[["splines"]]
  value <- str_c("Variance = ", 
                 format(round(x1[["Estimate"]], 2),2),
                 "; 95% CI = ",
                 format(round(x1[["l-95% CI"]],2),2),
                 ", ", 
                 format(round(x1[["u-95% CI"]],2),2))
  value
}

## get variance parameters for % ATN breastfeeding for CRP and IL-8
get.splinecoef <- function(df,a,b){
  tbl <- df %>% filter(Measure == a, `% ATN Breastfeeding` == b) 
  tbl$`Predicted Value` 
}

get.covar <- function(x){
  pump_df <- as.data.frame(x$ifm24hr_allpump_pct) %>% mutate(covar = "% Pumping") 
  tsd_df <- as.data.frame(x$tsd) %>% mutate(covar = "Days Since Delivery")
  covars_df <- full_join(pump_df, tsd_df)
  covars_df
}
