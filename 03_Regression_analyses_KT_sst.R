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

###########################################
### March site data by discrete SIZE CLASS#
###########################################
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
    Latitude= first(LATITUDE),
    LONGITUDE = first(LONGITUDE),
    .groups = "drop")%>%
  mutate(
    prop_mean_PM_adj = case_when(  #way to adjust 3
      prop_mean_PM == 0 ~ 0.01,
      prop_mean_PM == 1 ~ 0.91,
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
#SST_Mean:MED: coefficient=26.831 pvalue=0.025    

############################################
#look at residuals. first simulate w DHARMa#
############################################

sim_res <- simulateResiduals(fittedModel = model, plot = TRUE)
#no significant deviations or problems detected
hist(sim_res$scaledResiduals, main = "Histogram of Scaled Residuals", xlab = "Residuals")

plotResiduals(sim_res, site2$SST_Mean)
#plotResiduals(sim_res, site2$SST_range)

testUniformity(sim_res)     # Are residuals uniformly distributed?
#D = ns, KS test p = 0.53
testDispersion(sim_res)     # Is there overdispersion?
#p=0.528
testOutliers(sim_res)       # Are there extreme values?
#ns

#plot resudials by site
plotResiduals(sim_res, site2$SITE)
#ns

#Visualize interactions
site2
small_data <- subset(site2, TAIL_BINS == "Q20")
newdata_small<-small_data
newdata_small$SST_range <- mean(small_data$SST_range)  # Set SST_range to mean
# newdata_small$SST_mean <- seq(min(small_data$SST_Mean), 
#                                      max(small_data$SST_Mean), 
#                                      by = round(diff(range(small_data$SST_Mean)), 3) / nrow(small_data))
newdata_small$SST_mean <- seq(min(small_data$SST_Mean), 
                              max(small_data$SST_Mean), 
                              length.out = nrow(newdata_small)) 
#this would be predicted PM , create lower and upper CI
p <- predict(best.mod, newdata = newdata1, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata1<-cbind(newdata1,p)
newdata1$Predict.lwr <- newdata1$Predicted_Juv - 1.96 * newdata1$SE_Juv # confidence interval upper bound
newdata1$Predict.upr <- newdata1$Predicted_Juv + 1.96 * newdata1$SE_Juv # confidence interval lower bound
newdata1$HSts_cat<-"0-3 years"

