#PLOTTING

## Figure 1
fig1_df1 <- primpred_df %>% filter(measure %in% c("CRP","IL-8"))
fig1_df2 <- primpred_df %>% filter(measure %in% c("IL-6", "IL-1ß", "TNF-α"))

fig1a <- get.plot(fig1_df1, fig1_df1$ifm24hr_atnbf_pct) +
  xlab("") + 
  ylab("Evening-to-Morning \n Delta Secretion Rate")+
  scale_fill_manual(values = c("#BD973D", "#5F5C29"))+
  scale_color_manual(values = c("#BD973D", "#5F5C29")) 

fig1b <- get.plot(fig1_df2, fig1_df2$ifm24hr_atnbf_pct) +
  scale_fill_manual(values = c("#3B7D6E", "#5792CC", "#4D5B75")) +
  scale_color_manual(values = c("#3B7D6E", "#5792CC", "#4D5B75")) +
  labs(x = "% ATN Breastfeeding (24 hrs)",
       y = "Evening-to-Morning \n Delta Secretion Rate",
       caption = str_wrap("Fig 1. Predicted evening-to-morning change in CRP, IL-8, IL-6, IL-1ß, and TNF-α secretion rates by % ATN breastfeeding. Solid lines = point estimates for predicted median value. Shaded regions = 95% credible intervals. Dotted lines = zero difference between evening and morning secretion rate. Values above the horizontal dotted line = secretion rate is higher in the morning. Values below the horizontal dotted line = secretion rate is higher in the evening.",145)) 
  
figure_1 <- plot_grid(fig1a, fig1b, ncol=1, rel_heights=c(1,0.75))

ggsave("./output/figure_1.png",figure_1, 
        device = ragg::agg_png,dpi = 600,
        width = 8, height = 6, units = "in")

## Figure 2
figure_2 <- ggplot(tsdpred_df %>% filter(measure == "IL-8")) +
  geom_ribbon(aes(x = ifm24hr_atnbf_pct, y = Estimate, fill = tsd,
                  ymin = Q5, ymax = Q95),
              alpha = 0.5, colour = NA) + 
  geom_line(aes(x = ifm24hr_atnbf_pct, y = Estimate, color = tsd)) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.3, linetype = "longdash") +
  facet_grid(measure ~ tsd, scales = "fixed")+
  guides(fill="none", color = "none") +
  scale_fill_manual(values = c("#5F5C29","#5F5C29","#5F5C29")) + 
  scale_color_manual(values = c("#5F5C29","#5F5C29","#5F5C29")) +
  theme(plot.caption = element_text(hjust = 0),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") + 
  labs(x = "% ATN Breastfeeding (24 hrs)",
       y = "Evening-to-morning \n Delta Secretion Rate", 
       caption = str_wrap("Fig 2. Predicted median evening-to-morning change in IL-8 by % ATN breastfeeding and time since delivery. Solid lines = point estimates for predicted median value. Shaded regions = 95% credible intervals. Dotted lines = zero difference between evening and morning secretion rate. Values above the horizontal dotted line = secretion rate is higher in the morning. Values below the horizontal dotted line = secretion rate is higher in the evening.",145))

ggsave("./output/figure_2.png",figure_2, 
       device = ragg::agg_png,dpi = 600,
       width = 8, height = 3, units = "in")

### Figure 3
figure_3 <- ggplot(pumppred_df %>% filter(measure == "CRP")) +
  geom_ribbon(aes(x = ifm24hr_atnbf_pct, y = Estimate, fill = pump,
                  ymin = Q5, ymax = Q95),
              alpha = 0.5, colour = NA) + 
  geom_line(aes(x = ifm24hr_atnbf_pct, y = Estimate, color = pump)) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.3, linetype = "longdash") + 
  facet_grid(measure ~ pump, scales = "fixed")+
  guides(fill="none", color = "none") +
  scale_fill_manual(values = c("#BD973D", "#BD973D", "#BD973D"))+
  scale_color_manual(values = c("#BD973D", "#BD973D", "#BD973D"))+
  theme(plot.caption = element_text(hjust = 0),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") + 
  labs(x = "% ATN Breastfeeding (24 hrs)",
       y = "Evening-to-morning \n Delta Secretion Rate",
       caption = str_wrap("Fig 3. Predicted median evening-to-morning change in CRP by % ATN breastfeeding at 0%, 50% and 100% pumping. Solid lines = point estimates for predicted median value. Shaded regions = 95% credible intervals. Dotted lines = zero difference between evening and morning secretion rate. Values above the horizontal dotted line = secretion rate is higher in the morning. Values below the horizontal dotted line = secretion rate is higher in the evening.",145))

ggsave("./output/figure_3.png",figure_3, 
       device = ragg::agg_png,dpi = 600,
       width = 8, height = 3, units = "in")

##-------------------------------------------------------------------------------
# SUPPLEMENTAL FIGURES

## lookup table for variable names
lookup_tbl <- c(
  atnbf = "ATN \n breastfeeding",
  liquids  = "Non-breastmilk \n liquids",
  allpump = "All pumped",
  pumped = "Fresh pumped",
  solids = "Solid foods",
  storedpumped = "Stored pumped",
  ifm24hr_allpump_pct = "All pumped",
  ifm24hr_atnbf_pct = "ATN \n breastfeeding"
)

## Figure S1
fig_S1_df <- survey_clean %>%
  select(pid, contains(c("_pct","ifm2wks"))) %>%
  group_by(pid) %>%
  pivot_longer(ifm24hr_liquids_pct:ifm2wks_allpump) %>%
  separate(name, c("timing","ifm")) %>%
  pivot_wider(names_from = timing, values_from = value) %>%
  filter(!ifm %in% c("donated","futurepumped"))

fig_S1_df$ifm <- unname(lookup_tbl[fig_S1_df$ifm])

figure_S1 <- ggplot(fig_S1_df, aes(x = ifm24hr, y = ifm2wks, color = ifm)) + 
  geom_smooth(method = 'lm')+ 
  geom_point(position= "jitter") + 
  facet_grid(~ ifm) + 
  scale_fill_manual(values = cal_palette("kelp1"))+
  scale_color_manual(values = cal_palette("kelp1")) + 
  guides(fill="none", color = "none") + 
  stat_cor(aes(label = after_stat(rr.label)), 
           size = 3, color = "darkgrey", geom = "label") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") + 
  labs(x = "% Infant Feedings (24 hrs)",
       y = "% Infant Feedings (2 weeks)",
       caption = str_wrap("Fig S1. Correlation between reported reliance on infant feeding method during the 24-hour collection period versus the prior 2 weeks. While participants were able to indicate multiple methods for each feeding bout in the 24-hour colleciton period, the question regarding the preceding two weeks was phrased differently and required each percentage to add up to 100%.",145))

ggsave("./output/figure_S1.png",figure_S1, 
       device = ragg::agg_png,dpi = 600,
       width = 8, height = 3, units = "in")

## Figure S2
fig_S2_df <- bio_df %>%
  filter(measure == "crp") %>%
  select(pid, sample, flow_rate) %>%
  full_join(., survey_clean %>% select(pid, ifm24hr_atnbf_pct, 
                                       ifm24hr_allpump_pct)) %>%
  filter(!is.na(sample)) %>%
  group_by(pid, sample) %>%
  pivot_longer(ifm24hr_atnbf_pct:ifm24hr_allpump_pct, names_to = "ifm") %>%
  mutate(sample_adj = case_when(sample == 1 ~ "Bedtime Sample",
                                sample ==2 ~ "Waking Sample"))

fig_S2_df$ifm <- unname(lookup_tbl[fig_S2_df$ifm])

figure_S2 <- ggplot(
  fig_S2_df, aes(x = value, y = log(flow_rate), color = sample_adj)) +
  geom_smooth(method = 'lm') + 
  geom_point(position = "jitter") + 
  facet_grid(ifm ~ sample_adj) + 
  guides(fill="none", color = "none")+
  scale_fill_manual(values = cal_palette("bigsur2"))+
  scale_color_manual(values = cal_palette("bigsur2"))+
  stat_cor(aes(label = after_stat(rr.label)), 
           size = 3, color = "darkgrey", geom = "label") + 
  theme(plot.caption = element_text(hjust = 0),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") + 
  labs(x = "% Infant Feedings (24 hrs)",
       y = "Log Flow Rate",
       caption = str_wrap("Fig S2. Log flow rate by % ATN breastfeeding and % pumping by sample (bedtime versus waking).",145))

ggsave("./output/figure_S2.png",figure_S2, 
       device = ragg::agg_png,dpi = 600,
       width = 6, height = 4, units = "in")

#-------------------------------------------------------------------------------
## Figures S3 and Figures S4

#Plotting conditional effects of % pumping and tsd
cond_effects <- map(mod_list, conditional_effects)

cond_list <- map(cond_effects, get.covar)

cond_df <- bind_rows(cond_list, .id = "measure") %>%
  mutate(measure = ordered(
    measure, levels = c("CRP", "IL-8", "IL-6", "IL-1ß", "TNF-α"))) %>%
  rename(Estimate = estimate__, Q5 = lower__, Q95 = upper__) %>%
  select(measure, Estimate, Q5, Q95, covar, ifm24hr_allpump_pct, tsd) 

figs3_df <- cond_df %>% filter(covar == "% Pumping")
figs4_df <- cond_df %>% filter(covar == "Days Since Delivery")

## Figure S3
figure_S3 <- get.plot(a = figs3_df, b = figs3_df$ifm24hr_allpump_pct) + 
  scale_fill_manual(values = cal_palette("sierra1"))+
  scale_color_manual(values = cal_palette("sierra1")) +
  theme(plot.caption = element_text(hjust = 0),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") + 
  labs(x = "% Pumping (24 hrs)",
       y = "Evening-to-morning \n Delta Secretion Rate",
       caption = str_wrap("Fig S3. Predicted median evening-to-morning change in CRP, IL-8, IL-6, IL-1ß, and TNF-α secretion rates by % pumping. Solid lines = point estimates for predicted median value. Shaded regions = 95% credible intervals. Dotted lines = zero difference between evening and morning secretion rate. Values above the horizontal dotted line = secretion rate is higher in the morning. Values below the horizontal dotted line = secretion rate is higher in the evening.",145))

ggsave("./output/figure_S3.png",figure_S3, 
       device = ragg::agg_png,dpi = 600,
       width = 8, height = 3, units = "in")

## Figure S4
figure_S4 <- get.plot(a = figs4_df, b = figs4_df$tsd) + 
  scale_fill_manual(values = cal_palette("sierra1")) +
  scale_color_manual(values = cal_palette("sierra1")) +
  theme(plot.caption = element_text(hjust = 0),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") + 
  labs(x = "Days since delivery",
       y = "Evening-to-morning \n Delta Secretion Rate",
       caption = str_wrap("Fig S4. Predicted median evening-to-morning change in CRP, IL-8, IL-6, IL-1ß, and TNF-α secretion rates \n by days since delivery. Solid lines = point estimates for predicted median value. Shaded regions = 95% credible intervals. Dotted lines = zero difference between evening and morning secretion rate. Values above the horizontal dotted line = secretion rate is higher in the morning. Values below the horizontal dotted line = secretion rate is higher in the evening.",145))

ggsave("./output/figure_S4.png",figure_S4, 
       device = ragg::agg_png,dpi = 600,
       width = 8, height = 3, units = "in")
