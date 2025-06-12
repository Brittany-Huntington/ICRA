library(dplyr)
library(ggplot2)

# 1. Select and scale predictors
sub <- eds %>%
  select(SITE,
         lat,
         DHW_Mean = DHW.MeanMax_Degree_Heating_Weeks_jplMUR_Daily_YR01,
         DHW_Mean_Major = DHW.MeanMax_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
         DHW_Dur = DHW.MeanDur_Degree_Heating_Weeks_jplMUR_Daily_YR01,
         DHW_Dur_Major = DHW.MeanDur_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
         DHW_Max_Major = DHW.MaxMax_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
         DHW_Mean_CRW = DHW.MeanMax_Degree_Heating_Weeks_CRW_Daily_YR01,
         DHW_Mean_Major_CRW = DHW.MeanMax_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
         DHW_Dur_CRW = DHW.MeanDur_Degree_Heating_Weeks_CRW_Daily_YR01,
         DHW_Dur_Major_CRW = DHW.MeanDur_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
         DHW_Max_Major_CRW = DHW.MaxMax_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
         SST_AnnRange_CRW = mean_annual_range_Sea_Surface_Temperature_CRW_Daily_YR01,
         SST_MonthRange_CRW = mean_monthly_range_Sea_Surface_Temperature_CRW_Daily_YR01,
         SST_Mean_CRW = mean_Sea_Surface_Temperature_CRW_Daily_YR01,
         SST_Q05_CRW = q05_Sea_Surface_Temperature_CRW_Daily_YR01,
         SST_Q95_CRW = q95_Sea_Surface_Temperature_CRW_Daily_YR01,
         SST_SD_CRW = sd_Sea_Surface_Temperature_CRW_Daily_YR01,
         SST_BiweekRange_CRW = mean_biweekly_range_Sea_Surface_Temperature_CRW_Daily_YR01,
         SST_AnnRange = mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         SST_MonthRange = mean_monthly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         SST_Mean = mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         SST_Q05 = q05_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         SST_Q95 = q95_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         SST_SD = sd_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         SST_BiweekRange = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)

# 2. Scale predictors (excluding SITE and lat)
preds_scaled <- sub %>%
  select(-SITE, -lat) %>%
  scale(center = TRUE, scale = TRUE) %>%
  as.data.frame()

# Add SITE column back
preds_scaled$SITE <- sub$SITE

# Rename columns to indicate scaling
names(preds_scaled)[1:(ncol(preds_scaled)-1)] <- paste0("scaled_", names(preds_scaled)[1:(ncol(preds_scaled)-1)])

# 3. Merge with colony-level PM data
merged_PM_colony <- use_sub %>%
  left_join(ICRA_PM, by = "SITE") %>%
  drop_na(PER_DEAD)

# Subset by size class
small <- merged_PM_colony %>% filter(TAIL_BINS == "Q20")
med   <- merged_PM_colony %>% filter(TAIL_BINS == "QMED")
large <- merged_PM_colony %>% filter(TAIL_BINS == "Q80")

# 4. Join scaled predictors to each group
small.df <- small %>% left_join(preds_scaled, by = "SITE")
med.df   <- med   %>% left_join(preds_scaled, by = "SITE")
large.df <- large %>% left_join(preds_scaled, by = "SITE")

# 5. Define predictor names (all columns starting with 'scaled_')
predictor_vars <- names(preds_scaled)[grepl("^scaled_", names(preds_scaled))]

# 6. Loop through predictors and plot by size class
groups <- list(Small = small.df, Medium = med.df, Large = large.df)

for (group_name in names(groups)) {
  df <- groups[[group_name]]
  
  for (pred in predictor_vars) {
    df_plot <- df %>% filter(!is.na(.data[[pred]]), !is.na(PER_DEAD))
    
    if (nrow(df_plot) > 10) {  # Only plot if sufficient data
      p <- ggplot(df_plot, aes_string(x = pred, y = "PER_DEAD")) +
        geom_jitter(width = 0.1, alpha = 0.4, color = "blue") +
        geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
        labs(title = paste(group_name, "colonies — PER_DEAD vs", pred),
             x = pred, y = "% Dead Coral (PER_DEAD)") +
        theme_minimal()
      
      print(p)
    }
  }
}
