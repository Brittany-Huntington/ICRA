rm(list = ls())
library(tidyverse)
library(corrplot)   
library(GGally)    
library(betareg)
library(statmod)
library(lmtest)
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(DHARMa)
#exploring if spatial variation in SST mean and duration correlates w PM


icra2025eds<-read.csv("merged_PM_colony_all_YR01.csv")

n <- nrow(icra2025eds)

small<- icra2025eds%>%
  filter(TAIL_BINS == "Q20")%>%
  group_by(SITE) %>%
  summarise(
    mean_PM = mean(PER_DEAD, na.rm = TRUE),
    prop_mean_PM = mean_PM / 100,
    prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n,
    SST_Mean = first(mean_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    SST_range = first(mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    .groups = "drop"
  )


  
med<- icra2025eds%>%
  filter(TAIL_BINS == "QMED")%>%
  group_by(SITE) %>%
  summarise(
    mean_PM = mean(PER_DEAD, na.rm = TRUE),
    prop_mean_PM = mean_PM / 100,
    prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n,
    SST_Mean = first(mean_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    SST_range = first(mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    .groups = "drop"
  )


large<- icra2025eds%>%
  filter(TAIL_BINS == "Q80")%>%
  group_by(SITE) %>%
  summarise(
    mean_PM = mean(PER_DEAD, na.rm = TRUE),
    prop_mean_PM = mean_PM / 100,
    prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n,
    SST_Mean = first(mean_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    SST_range = first(mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    .groups = "drop"
  )

#######################################################################################
### 1. looking at March site data by discrete SIZE CLASS #
#######################################################################################
small_model<- glmmTMB(
  prop_mean_PM_adj ~ (SST_Mean + SST_range), #+ (1 | SITE),
  data = small,
  family = beta_family()
)

med_model<- glmmTMB(
  prop_mean_PM_adj ~ (SST_Mean + SST_range), #+ (1 | SITE),
  data = med,
  family = beta_family()
)

large_model<- glmmTMB(
  prop_mean_PM_adj ~ (SST_Mean + SST_range), #+ (1 | SITE),
  data = large,
  family = beta_family()
)
summary(small_model)
summary(med_model)
summary(large_model)

#######################################################################################
### 1. looking at March site data by discrete SIZE CLASS #
#######################################################################################
site<-icra2025eds%>%
  filter(!is.na(TAIL_BINS)) 

  
site2<-site%>%
  group_by(SITE, TAIL_BINS)%>%
  summarise(
    n = n(),
    mean_PM = mean(PER_DEAD, na.rm = TRUE),
    prop_mean_PM = mean_PM / 100,
    #prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.1) / n, #way to adjust 1
    # epsilon = 1e-6, #way to adjust 
    # prop_mean_PM_adj = pmin(pmax(prop_mean_PM, epsilon), 1 - epsilon), #way to adjust 2
    SST_Mean = first(mean_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    SST_range = first(mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    .groups = "drop")%>%
  mutate(
    prop_mean_PM_adj = case_when(  #way to adjust 3
      prop_mean_PM == 0 ~ 0.01,
      prop_mean_PM == 1 ~ 0.99,
      TRUE ~ prop_mean_PM
    )
  )

hist(site2$prop_mean_PM, breaks = 30)
hist(site2$prop_mean_PM_adj, breaks = 30)


model<- glmmTMB(
  prop_mean_PM_adj ~ (SST_Mean * TAIL_BINS) + (SST_range * TAIL_BINS), #+ (1 | SITE),
  data = site2,
  family = beta_family()
)
summary(model)

