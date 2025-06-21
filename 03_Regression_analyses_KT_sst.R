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
library(rgdal)
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
#SST_Mean:MED: coefficient=26.831 pvalue=0.025. close to brittany's

performance::r2(model)
# R2 for Generalized Linear Regression
#Ferrari's R2: 0.881



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

################
#plot raw data##
################


site2$Size_cat <- recode(site2$TAIL_BINS,
                         "Q20" = "Small",
                         "QMED" = "Medium",
                         "Q80" = "Large")
colors<-c("cyan4","purple3","gray67")

site2$Size_cat <- factor(site2$Size_cat, levels = c("Small", "Medium", "Large"))

geom_smooth(method = "lm", se = TRUE)



#loess
ggplot(site2, aes(x = SST_Mean, y = prop_mean_PM_adj, color = TAIL_BINS)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess",  se = TRUE) +
  facet_wrap(~ Size_cat) +
  theme_bw() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(
    x = expression(bold("SST Mean ("*~degree*C*")")),
    y = expression(bold("Proportional Mortality")),
    title = "Observed PM by SST Mean Across Size Classes"
  ) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

############################
## Visualize interactions ##
############################

####Step 1: create 3 new data frames for time since heat stress interval (or size bin our case) and hold everything but variable of interest constant####
#set SST range to mean (holding it constant)
small_data <- subset(site2, TAIL_BINS == "Q20")
newdata_small<-small_data
newdata_small$SST_range <- mean(small_data$SST_range)  # Set SST_range to mean
newdata_small$SST_mean <- seq(min(small_data$SST_Mean), 
                              max(small_data$SST_Mean), 
                              length.out = nrow(newdata_small)) # making the sequence matches the number of rows. courtney used by = round(diff(range(small_data$SST_Mean)), 3) / nrow(small_data))

# med_data <- subset(site2, TAIL_BINS == "QMED")
# newdata_med<-med_data
# newdata_med$SST_range <- mean(med_data$SST_range)  # Set SST_range to mean
# newdata_med$SST_mean <- seq(min(med_data$SST_Mean),
#                               max(med_data$SST_Mean),
# #                               length.out = nrow(newdata_med))

med_data <- subset(site2, TAIL_BINS == "QMED")
newdata_med<-med_data
newdata_med$SST_range <- mean(med_data$SST_range)  # Set SST_range to mean
newdata_med$SST_mean <- seq(min(med_data$SST_Mean),
                            max(med_data$SST_Mean),
                            by=round(rg(med_data$SST_Mean),3)/nrow(med_data))


large_data <- subset(site2, TAIL_BINS == "Q80")
newdata_large<-large_data
newdata_large$SST_range <- mean(large_data$SST_range)  # Set SST_range to mean
newdata_large$SST_mean <- seq(min(large_data$SST_Mean), 
                            max(large_data$SST_Mean), 
                            length.out = nrow(newdata_large)) 
newdata_large$SST_mean <- seq(min(large_data$SST_Mean), 
                              max(large_data$SST_Mean), 
                              length.out = nrow(newdata_large)) 

####Step 2: predict PM as a function of SST_mean using the model output (e.g. model in this case)####
#this would be predicted PM , create lower and upper CI
p <- predict(model, newdata = newdata_small, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_PM","SE_PM")
newdata_small<-cbind(newdata_small,p)
newdata_small$Predict.lwr <- newdata_small$Predicted_PM - 1.96 * newdata_small$SE_PM # confidence interval upper bound
newdata_small$Predict.upr <- newdata_small$Predicted_PM + 1.96 * newdata_small$SE_PM # confidence interval lower bound
newdata_small$Size_cat<-"small"

p <- predict(model, newdata = newdata_med, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_PM","SE_PM")
newdata_med<-cbind(newdata_med,p)
newdata_med$Predict.lwr <- newdata_med$Predicted_PM - 1.96 * newdata_med$SE_PM # confidence interval upper bound
newdata_med$Predict.upr <- newdata_med$Predicted_PM + 1.96 * newdata_med$SE_PM # confidence interval lower bound
newdata_med$Size_cat<-"med"

p <- predict(model, newdata = newdata_large, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_PM","SE_PM")
newdata_large<-cbind(newdata_large,p)
newdata_large$Predict.lwr <- newdata_large$Predicted_PM - 1.96 * newdata_large$SE_PM # confidence interval upper bound
newdata_large$Predict.upr <- newdata_large$Predicted_PM + 1.96 * newdata_large$SE_PM # confidence interval lower bound
newdata_large$Size_cat<-"large"

names(newdata_small)
names(newdata_med)
names(newdata_large)
#Merge into 1 dataframe and add column for each size category.  THIS didnt work bc diff rows : 
all.newdata<-rbind(newdata_small,newdata_med,newdata_large)


#Reorder size variables
all.newdata$Size_cat <- factor(all.newdata$Size_cat, levels = c("small","med","large"))
all.newdata<- all.newdata[order(all.newdata$Size_cat),];head(all.newdata)


#this is the funky graph.. don't know the issue
ggplot() +
  geom_line(data = all.newdata, aes(x = SST_mean, y = Predicted_PM, color = Size_cat), size = 1) +
  geom_ribbon(data = all.newdata, aes(x = SST_mean, ymin = Predict.lwr, ymax = Predict.upr, fill = Size_cat), alpha = 0.1) +
  facet_wrap(~Size_cat)+
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(1.5, 'cm'),
    legend.text = element_text(size = 14),
    text = element_text(size = 18),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  ) +
  ylab(expression(bold("Predicted PM"))) +
  xlab(expression(bold("SST Mean ("*~degree*C*")"))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  geom_rug(data = all.newdata, mapping = aes(x = SST_mean, y = 0))


#here is a better looking graph
ggplot(all.newdata, aes(x = SST_Mean, y = Predicted_PM, color = Size_cat, fill =Size_cat)) +
  geom_line() +
  geom_ribbon(data = all.newdata,aes(x = SST_Mean,ymin = Predict.lwr, ymax = Predict.upr),alpha = 0.1)+
  labs(title = "Predicted  PM for Different Size Classes",
       x = "SST Mean", 
       y = "Predicted PM") +
  theme_minimal() +
  scale_color_manual(values = c("small" = "cyan4", "med" = "purple3", "large" = "gray67"))+
  scale_fill_manual(values = c("small" = "cyan4", "med" = "purple3", "large" = "gray67"))+
  geom_rug(data=all.newdata,mapping=aes(x=SST_Mean,y=0))

#From Brittany (haven't updated her code to work with my script)
# Get model predictions across SST_mean, grouped by TAIL_BINS
preds <- ggpredict(site2, terms = c("SST_mean", "TAIL_BINS"))

# Plot
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, color = NA) +
  labs(x = "SST Mean",
       y = "Predicted Partial Mortality (mean_PM)",
       color = "TAIL_BINs", fill = "TAIL_BINs") +
  theme_minimal()
