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
#exploring if spatial variation in DHW mean and duration correlates w PM

#########################################################
###1.looking at March only site data separated by size bin#
#########################################################
small<-read.csv("small_ICRA_avg.csv")
med<-read.csv("med_ICRA_avg.csv")
large<-read.csv("large_ICRA_avg.csv")

#first need to convert percent PM to proportion and make any zeros = 0.5 (3 sites has 0 PM) for betareg
#and scale PM and preds (subtract mean and divide by sd)
small<-small%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur),
         prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n)

med<-med%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur),
         prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n)

large<-large%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur),
         prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n)

small_model_glmm <- glmmTMB(
  prop_mean_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) #+ (1 | SITE),
  data = small,
  family = beta_family()
)


med_model_glmm <- glmmTMB(
  prop_mean_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) #+ (1 | SITE),
  data = med,
  family = beta_family()
)

large_model_glmm <- glmmTMB(
  prop_mean_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur), #+ (1 | SITE),
  data = large,
  family = beta_family()
)

# Summary with coefficients and significance
summary(small_model_glmm)
summary(med_model_glmm)
summary(large_model_glmm)

#small model only has significance

#dharma, stimulate resudials
#sim_res <- simulateResiduals(fittedModel = large_model_glmm, plot = TRUE)


#linear model beta regression (not great bc we have multiple colonies at one site)
small_model <- betareg(prop_mean_PM_adj ~ scaled_DHW_Mean + scaled_DHW_Dur, data = small)
med_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = med)
large_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = large)

# Summary with coefficients and significance
summary(small_model)
summary(med_model)
summary(large_model)

ggplot(large, aes(x = scaled_DHW_Mean, y = prop_mean_PM)) +
  geom_point() +
  geom_smooth(method = "loess")  # or method = "glm", method.args = list(family = binomial)


#nothing 

####################################################
### 2.looking at March + Feb site data, no sizes#######
####################################################
south_sites<-read.csv("merged2025_PM_S_site.csv")

#calculate proportion instead of percent PM and scale
south_sites<-south_sites%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur),
         prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n)


site_model <- glmmTMB(
  prop_mean_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur), #+ (1 | SITE),
  data = south_sites,
  family = beta_family()
)
summary(site_model)
#not significant

#linear model
#site_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = south_sites)

#nothing


###########################################################################
### 3. looking at March + Feb site data, applying a >10% PM threshold #########
###########################################################################
south_colonies<-read.csv("merged2025_PM_S_colony.csv")

#exlucing colonies that had <10% mortality, recalculate mean PM, convert to prop + scale
threshold <- south_colonies %>%
  filter(PER_DEAD >= 10) 

n <- nrow(threshold_site)

threshold_site<-threshold%>%
  group_by(SITE) %>%
  summarise(
    mean_PM = mean(PER_DEAD, na.rm = TRUE),
    DHW_Mean = first(DHW_Mean),
    DHW_Dur = first(DHW_Dur),
    .groups = "drop"
    )

threshold_site_scaled<-threshold_site%>%    
      mutate(prop_mean_PM = mean_PM / 100,
             scaled_DHW_Mean = scale(DHW_Mean),
             scaled_DHW_Dur = scale(DHW_Dur),
             prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.5) / n)
  
threshold_site_model<- glmmTMB(
  prop_mean_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur), #+ (1 | SITE),
  data = threshold_site_scaled,
  family = beta_family()
)
summary(threshold_site_model)
#not significant

#linear model
#threshold_site_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = threshold_site_scaled)


ggplot(threshold_site_scaled, aes(x = scaled_DHW_Mean, y = prop_mean_PM)) +
  geom_point() +
  geom_smooth(method = "loess")  # or method = "glm", method.args = list(family = binomial)

#looking at interaction
betareg(prop_mean_PM ~ scaled_DHW_Mean * scaled_DHW_Dur, data = threshold_site_scaled)

#Look at polynomials
betareg(prop_mean_PM ~ poly(scaled_DHW_Mean, 2) + poly(scaled_DHW_Dur, 2), data = threshold_site_scaled)


# Create prediction data
new_data <- threshold_site_scaled %>%
  tidyr::expand(
    scaled_DHW_Mean = seq(min(scaled_DHW_Mean), max(scaled_DHW_Mean), length.out = 100),
    scaled_DHW_Dur = mean(scaled_DHW_Dur)  # hold one constant
  )

new_data$predicted_PM <- predict(
  object = betareg(prop_mean_PM ~ poly(scaled_DHW_Mean, 2) + poly(scaled_DHW_Dur, 2), 
                   data = threshold_site_scaled),
  newdata = new_data,
  type = "response"
)

# Plot
ggplot(new_data, aes(x = scaled_DHW_Mean, y = predicted_PM)) +
  geom_line(color = "blue") +
  labs(x = "Scaled DHW Mean", y = "Predicted Proportion PM") +
  theme_minimal()

ggplot(threshold_site_scaled, aes(x = scaled_DHW_Mean, y = prop_mean_PM)) +
  geom_point() +
  geom_smooth(method = "loess")  # or method = "glm", method.args = list(family = binomial)


##########################################
### 4. looking at March + Feb COLONY data#
##########################################

#adjust 0 and 100s, scale preds
n_obs <- nrow(south_colonies)

south_colonies_scaled <- south_colonies %>%
  mutate(
    prop_PM = PER_DEAD / 100,
    prop_PM_adj = (prop_PM * (n_obs - 1) + 0.5) / n_obs,
    scaled_DHW_Mean = scale(DHW_Mean),
    scaled_DHW_Dur = scale(DHW_Dur)
  )
colnames(south_colonies_scaled)

colonies_site_model_wsite<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) + (1 | SITE),
  data = south_colonies_scaled,
  family = beta_family()
)

summary(colonies_site_model_wsite)
#not significant

colonies_site_model<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur), #+ (1 | SITE),
  data = south_colonies_scaled,
  family = beta_family()
)

summary(colonies_site_model)
#SIGNIFICANCE of DHW_Dur 

#linear model
#colonies_site_model <- betareg(prop_PM_adj ~ scaled_DHW_Mean + scaled_DHW_Dur, data = south_colonies_scaled)

#model only explains 2% ov variance. But, 
######## DHW_Dur is strongly correlated w PM ###############

colonies_site_model_x <- betareg(prop_PM_adj ~ scaled_DHW_Mean * scaled_DHW_Dur, data = south_colonies_scaled)
summary(colonies_site_model_x)
#no interaction effect

#look at residuals. first simulate w DHARMa
sim_res <- simulateResiduals(fittedModel = colonies_site_model, plot = TRUE)
#significant deviation, meaning model misspecification, unmodeled structure, or a nonlinear relationship not captured by your predictors

hist(sim_res$scaledResiduals, main = "Histogram of Scaled Residuals", xlab = "Residuals")


plotResiduals(sim_res, south_colonies_scaled$scaled_DHW_Dur)
plotResiduals(sim_res, south_colonies_scaled$scaled_DHW_Mean)

testUniformity(sim_res)     # Are residuals uniformly distributed?
#D = 0.0956, p = 1.6e-09
#no, CHAT: Conclusion: Your model is not capturing the data’s structure fully. Something is missing or misspecified.
testDispersion(sim_res)     # Is there overdispersion?
#not extreme
testOutliers(sim_res)       # Are there extreme values?
#one outlier 

#plot resudials by site
plotResiduals(sim_res, south_colonies_scaled$SITE)

#suggests non-linearity. ok.so using Splines

library(splines)

model_nonlinear_spline <- glmmTMB(
  prop_PM_adj ~ ns(scaled_DHW_Dur, df = 3) + scaled_DHW_Mean + (1 | SITE),
  data = south_colonies_scaled,
  family = beta_family()
)

summary(model_nonlinear_spline)

model_nonlinear_spline2<- glmmTMB(prop_PM_adj ~ ns(scaled_DHW_Dur, 3) + ns(scaled_DHW_Mean, 3) + (1 | SITE),
data = south_colonies_scaled,
family = beta_family())

summary(model_nonlinear_spline2)

#or look at polynomial terms i.e. quadratic

model_nonlinear_poly <- glmmTMB(
  prop_PM_adj ~ poly(scaled_DHW_Dur, 2) + scaled_DHW_Mean + (1 | SITE),
  data = south_colonies_scaled,
  family = beta_family()
)

summary(model_nonlinear_poly)
AIC(colonies_site_model_wsite, model_nonlinear_spline, model_nonlinear_spline2, model_nonlinear_poly)

#linear has the lowest AIC....

sim_res_spline <- simulateResiduals(model_nonlinear_spline, plot = TRUE)
sim_res_spline2 <- simulateResiduals(model_nonlinear_spline2, plot = TRUE)


#########################################
### 5.looking at March + Feb COLONY data#
#########################################

#see if same result using only march data
march_colonies<-south_colonies_scaled%>%
  filter(!is.na(COLONYLENGTH))

march_colonies_site_model<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur), #+ (1 | SITE),
  data = march_colonies,
  family = beta_family()
)
summary(march_colonies_site_model)

#linear model
march_colonies_site_model <- betareg(prop_PM_adj ~ scaled_DHW_Mean + scaled_DHW_Dur, data = march_colonies)


#lost the effect


################################################
### 6. looking at March COLONY data BY SIZE#########
################################################

#check if size data interacts with environmental preds

small_colonies<-south_colonies_scaled%>%
  filter(TAIL_BINS=="Q20")

med_colonies<-south_colonies_scaled%>%
  filter(TAIL_BINS=="QMED")

large_colonies<-south_colonies_scaled%>%
  filter(TAIL_BINS=="Q80")

small_colony_model<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur), #+ (1 | SITE),
  data = small_colonies,
  family = beta_family()
)

med_colony_model<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur), #+ (1 | SITE),
  data = med_colonies,
  family = beta_family()
)

large_colony_model<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur), #+ (1 | SITE),
  data = large_colonies,
  family = beta_family()
)

summary(small_colony_model)
summary(med_colony_model)
summary(large_colony_model)

#linear models
small_colony_model <- betareg(prop_PM_adj ~ scaled_DHW_Mean + scaled_DHW_Dur, data = small_colonies)
med_colony_model <- betareg(prop_PM_adj ~ scaled_DHW_Mean + scaled_DHW_Dur, data = med_colonies)
large_colony_model <- betareg(prop_PM_adj ~ scaled_DHW_Mean + scaled_DHW_Dur, data = large_colonies)



#####within large colonies, significant correlation of DHW_mean with PM############
# others nothing.

#look at residuals
residuals_raw <- residuals(large_colonies, type = "response")
residuals_pearson <- residuals(large_colonies, type = "pearson")
residuals_rqr <- residuals(large_colonies, type = "quantile")# Randomized quantile residuals (recommended for beta regression)
#add residuals to df
large_colonies$residuals <- residuals_rqr

ggplot(large_colonies, aes(x = scaled_DHW_Mean, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Scaled DHW Mean", y = "Randomized Quantile Residuals") +
  theme_minimal()

ggplot(large_colonies, aes(x = scaled_DHW_Mean, y = prop_PM_adj)) +
  geom_point() +
  geom_smooth(method = "loess")  # or method = "glm", method.args = list(family = binomial)



##############################################################
### 7. looking at March COLONY data w continuous SIZE interaction#
##############################################################

#scale colonylength
south_colonies_scaled <- south_colonies_scaled %>%
  mutate(scaled_COLONYLENGTH = scale(COLONYLENGTH))

colony_model_by_size<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) * COLONYLENGTH, 
  data = south_colonies_scaled,
  family = beta_family()
)

summary(colony_model_by_size)

colony_model_by_size<-betareg(prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) * scaled_COLONYLENGTH, data = south_colonies_scaled)

#sig correlation of colony length and PM, but not any environmental preds
#also checked interaction of all but nothing


##################################################################
### 8. looking at March COLONY data w discrete SIZE CLASS interaction#
##################################################################

colony_model_by_class <- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) * TAIL_BINS + (1 | SITE),
  data = south_colonies_scaled,
  family = beta_family()
)
summary(colony_model_by_class)

colony_model_by_class<-betareg(prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) * TAIL_BINS, data = south_colonies_scaled)

#sig correlation of qmed and q80 with PM, nothing else

#IN SUMMARY
#when 

###################################################################################
### 9. looking at March COLONY data by discrete SIZE CLASS interaction with colonysize#
###################################################################################

small_colonies<-south_colonies_scaled%>%
  filter(TAIL_BINS=="Q20")

med_colonies<-south_colonies_scaled%>%
  filter(TAIL_BINS=="QMED")

large_colonies<-south_colonies_scaled%>%
  filter(TAIL_BINS=="Q80")

small_colony_modelcont<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) * scaled_COLONYLENGTH,
  data = small_colonies,
  family = beta_family()
)

med_colony_modelcont<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur)* scaled_COLONYLENGTH,
  data = med_colonies,
  family = beta_family()
)

large_colony_modelcont<- glmmTMB(
  prop_PM_adj ~ (scaled_DHW_Mean + scaled_DHW_Dur) * scaled_COLONYLENGTH,
  data = large_colonies,
  family = beta_family()
)

summary(small_colony_modelcont)
summary(med_colony_modelcont)
summary(large_colony_modelcont)


