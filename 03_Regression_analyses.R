rm(list = ls())
library(tidyverse)
library(corrplot)   
library(GGally)    
library(betareg)
library(statmod)
library(lmtest)
library(ggplot2)
library(dplyr)

small<-read.csv("small_ICRA_avg.csv")
med<-read.csv("med_ICRA_avg.csv")
large<-read.csv("large_ICRA_avg.csv")


merged_small <- sub_eds %>%
  left_join(small, by = "SITE")%>%
  drop_na(TAIL_BINS)

merged_med <- sub_eds %>%
  left_join(med, by = "SITE")%>%
  drop_na(TAIL_BINS)

merged_large <- sub_eds %>%
  left_join(large, by = "SITE")%>%
  drop_na(TAIL_BINS)

small<-small%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur),
         prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n))

#small has 3 sites with 0 PM in small corals. make the zeros 0.5


med<-med%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur))

large<-large%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur))

small_model <- betareg(prop_mean_PM_adj ~ scaled_DHW_Mean + scaled_DHW_Dur, data = small)

# Summary with coefficients and significance
summary(small_model)

med_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = med)
summary(med_model)

large_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = large)
summary(large_model)
