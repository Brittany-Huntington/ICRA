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
  mutate(
    PER_DEAD_TRUE = case_when(
      PER_DEAD == 0 ~ 0.001,    # small positive value instead of 0
      PER_DEAD == 100 ~ 99.999,    # instead of 100%
      TRUE ~ PER_DEAD
    )
  ) %>%                       # <-- added missing pipe here
  group_by(SITE) %>%
  summarise(
    mean_PM = mean(PER_DEAD_TRUE, na.rm = TRUE),
    SST_Mean = first(mean_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    SST_range = first(mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    .groups = "drop"
  ) %>%
  mutate(
    prop_mean_PM = mean_PM / 100
  )


  
med<- icra2025eds%>%
  filter(TAIL_BINS == "QMED")%>%
  mutate(
    PER_DEAD_TRUE = case_when(
      PER_DEAD == 0 ~ 0.001,    #instead of 0
      PER_DEAD == 100 ~ 99.999,    #instead of 100%
      TRUE ~ PER_DEAD
    )
  ) %>%                       
  group_by(SITE) %>%
  summarise(
    mean_PM = mean(PER_DEAD_TRUE, na.rm = TRUE),
    SST_Mean = first(mean_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    SST_range = first(mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    .groups = "drop"
  ) %>%
  mutate(
    prop_mean_PM = mean_PM / 100
  )


large <- icra2025eds %>%
  filter(TAIL_BINS == "Q80") %>%
  mutate(
    PER_DEAD_TRUE = case_when(
      PER_DEAD == 0 ~ 0.001,    #instead of 0
      PER_DEAD == 100 ~ 99.999,    #instead of 100%
      TRUE ~ PER_DEAD
    )
  ) %>%                       
  group_by(SITE) %>%
  summarise(
    mean_PM = mean(PER_DEAD_TRUE, na.rm = TRUE),
    SST_Mean = first(mean_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    SST_range = first(mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01),
    .groups = "drop"
  ) %>%
  mutate(
    prop_mean_PM = mean_PM / 100
  )

sd_PM <- sd(large$prop_mean_PM)

#######################################################################################
### 1. looking at March site data by discrete SIZE CLASS #
#######################################################################################
site<-icra2025eds%>%
  filter(!is.na(TAIL_BINS)) 

  
site2<-site%>%
  mutate(
    PER_DEAD = case_when(
      PER_DEAD == 0 ~ 0.001,    # small positive value instead of 0
      PER_DEAD == 100 ~ 99.999,    # instead of 100%
      TRUE ~ PER_DEAD
    )
  ) %>%                       
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
    .groups = "drop")
  # mutate(
  #   prop_mean_PM_adj = case_when(  #way to adjust after averaging 
  #     prop_mean_PM == 0 ~ 0.01,
  #     prop_mean_PM == 1 ~ 0.99,
  #     TRUE ~ prop_mean_PM
  #   )
  

hist(site$PER_DEAD, breaks = 30)
hist(site2$prop_mean_PM, breaks = 30)
hist(site2$prop_mean_PM_adj, breaks = 30)


model<- glmmTMB(
  prop_mean_PM ~ (SST_Mean * TAIL_BINS) + (SST_range * TAIL_BINS), #+ (1 | SITE),
  data = site2,
  family = beta_family()
)
summary(model)
#SST_Mean:MED: coefficient=26.831 pvalue=0.025. close to brittany's #prop_adj
#SST_mean:MED: 
performance::r2(model)
# R2 for Generalized Linear Regression
#Ferrari's R2: 0.881 #for mean_PM_adj

# R2 for Generalized Linear Regression
#Ferrari's R2: 0.657


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
#colors<-c("cyan4","purple3","gray67")
#colors<-c( "darkgrey", "#E1AD01",  "gold")
#colors<-c("grey", "orangered", "darkred" )

site2$Size_cat <- factor(site2$Size_cat, levels = c("Small", "Medium", "Large"))

geom_smooth(method = "lm", se = TRUE)



#loess
ggplot(site2, aes(x = SST_Mean, y = prop_mean_PM, color = Size_cat)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm",  se = TRUE) +
  facet_wrap(~ Size_cat) +
  theme_bw() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(
    x = expression(bold("SST Mean ("*~degree*C*")")),
    y = expression(bold("Observed Partial Mortality")),
   # title = "Observed PM by SST Mean Across Size Classes"
  ) +
  theme(
    legend.position = "none",
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

med_data <- subset(site2, TAIL_BINS == "QMED")
newdata_med<-med_data
newdata_med$SST_range <- mean(med_data$SST_range)  # Set SST_range to mean
newdata_med$SST_mean <- seq(min(med_data$SST_Mean),
                              max(med_data$SST_Mean),
                               length.out = nrow(newdata_med))

# med_data <- subset(site2, TAIL_BINS == "QMED")
# newdata_med<-med_data
# newdata_med$SST_range <- mean(med_data$SST_range)  # Set SST_range to mean
# newdata_med$SST_mean <- seq(min(med_data$SST_Mean),
#                             max(med_data$SST_Mean),
#                             by=round(rg(med_data$SST_Mean),3)/nrow(med_data)) #this package isn't supported anymore 


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
newdata_small$Size_cat<-"Small"

p <- predict(model, newdata = newdata_med, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_PM","SE_PM")
newdata_med<-cbind(newdata_med,p)
newdata_med$Predict.lwr <- newdata_med$Predicted_PM - 1.96 * newdata_med$SE_PM # confidence interval upper bound
newdata_med$Predict.upr <- newdata_med$Predicted_PM + 1.96 * newdata_med$SE_PM # confidence interval lower bound
newdata_med$Size_cat<-"Medium"

p <- predict(model, newdata = newdata_large, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_PM","SE_PM")
newdata_large<-cbind(newdata_large,p)
newdata_large$Predict.lwr <- newdata_large$Predicted_PM - 1.96 * newdata_large$SE_PM # confidence interval upper bound
newdata_large$Predict.upr <- newdata_large$Predicted_PM + 1.96 * newdata_large$SE_PM # confidence interval lower bound
newdata_large$Size_cat<-"Large"

names(newdata_small)
names(newdata_med)
names(newdata_large)
#Merge into 1 dataframe and add column for each size category.  THIS didnt work bc diff rows : 
all.newdata<-rbind(newdata_small,newdata_med,newdata_large)


#Reorder size variables
all.newdata$Size_cat <- factor(all.newdata$Size_cat, levels = c("Small","Medium","Large"))
all.newdata<- all.newdata[order(all.newdata$Size_cat),];head(all.newdata)


#ver1
ggplot() +
  geom_line(data = all.newdata, aes(x = SST_Mean, y = Predicted_PM, color = Size_cat), size = 1) +
  geom_ribbon(data = all.newdata, aes(x = SST_Mean, ymin = Predict.lwr, ymax = Predict.upr, fill = Size_cat), alpha = 0.1) +
  facet_wrap(~Size_cat)+
  geom_rug(data = all.newdata, mapping = aes(x = SST_Mean, y = 0, color = Size_cat), sides = "b", size = 1.5)+
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.key.size = unit(1.5, 'cm'),
    legend.text = element_text(size = 14),
    text = element_text(size = 18),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  ylab(expression(bold("Predicted PM"))) +
  xlab(expression(bold("SST Mean ("*~degree*C*")"))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)
)


#ver2
ggplot(all.newdata, aes(x = SST_Mean, y = Predicted_PM, color = Size_cat, fill = Size_cat)) +
  geom_line() +
  geom_ribbon(aes(ymin = Predict.lwr, ymax = Predict.upr), alpha = 0.3, color = NA) +
  facet_wrap(~Size_cat) +
  geom_rug(aes(x = SST_Mean, y = 0)) +
  labs(
    #title = "Predicted PM for Different Size Classes",
    x = expression(bold("SST Mean ("*~degree*C*")")),
    y = expression(bold("Predicted partial mortality"))
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    #legend.title = element_blank(),
    #legend.key.size = unit(0.5, 'cm'),
    #legend.text = element_text(size = 15),
    text = element_text(size = 15),
    #panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

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
