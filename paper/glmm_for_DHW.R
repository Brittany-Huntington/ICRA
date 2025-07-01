rm(list = ls())
library(tidyverse)
library(corrplot)   
library(GGally)    
library(eds)
library(betareg)
library(statmod)
library(lmtest)
library(ggplot2)
library(dplyr)

select = dplyr::select
rename  = dplyr::rename

#load("data/eds_output.Rdata")
#load("eds_output_Sep12025.Rdata")
load("eds_output_Sep12025_UPDATEDJPL.Rdata")
load(file ="data/ICRA_PM_SIZE_USE.Rdata")

#subset 2025 (can later add this to clean-colony level script)
s<-south_ICRA_survey_data %>%
  filter( YEAR == '2025')

select = dplyr::select
rename  = dplyr::rename

#subset 2025 from eds
#save the colnames as a file for ease in viewing variable names
column_names <- colnames(eds)
column_names_df <- data.frame(column_names)

#view just the 6month 1km data 
MO06_columns <- column_names[grepl("_jplMUR_Daily_MO06$", column_names)]
MO06_columns_df<- data.frame(MO06_columns)
non_zero_MO06_columns <- MO06_columns[colSums(eds[, MO06_columns] != 0) > 0]
print(non_zero_MO06_columns)

colnames(eds)
#subset jpm 2025, MO06 data
eds_MO06 <- eds %>%
  select(SITE, ends_with("jplMUR_Daily_MO06"))


eds_MO06 <- eds_MO06 %>%
  select(SITE,
         DHW_Mean = DHW.MeanMax_Degree_Heating_Weeks_jplMUR_Daily_MO06,
         DHW_MeanMax_Major = DHW.MeanMax_Major_Degree_Heating_Weeks_jplMUR_Daily_MO06,
         DHW_Dur = DHW.MeanDur_Degree_Heating_Weeks_jplMUR_Daily_MO06,
         DHW_Dur_Major = DHW.MeanDur_Major_Degree_Heating_Weeks_jplMUR_Daily_MO06,
         DHW_Max_Major = DHW.MaxMax_Major_Degree_Heating_Weeks_jplMUR_Daily_MO06,
         DHW_Max_Major5 = DHW.MaxMax_Major_Degree_Heating_Weeks_jplMUR_Daily_MO06,
         SST_AnnRange = mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_MO06,
         SST_MonthRange = mean_monthly_range_Sea_Surface_Temperature_jplMUR_Daily_MO06,
         SST_Mean = mean_Sea_Surface_Temperature_jplMUR_Daily_MO06,
         SST_Q05 = q05_Sea_Surface_Temperature_jplMUR_Daily_MO06,
         SST_Q95 = q95_Sea_Surface_Temperature_jplMUR_Daily_MO06,
         SST_SD = sd_Sea_Surface_Temperature_jplMUR_Daily_MO06,
         SST_BiweekRange = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_MO06)
         
#   select(-any_of(c("DHW.YearsToLast_Degree_Heating_Weeks_jplMUR_Daily_MO06", 
#                    "DHW.YearsToLast_Major_Degree_Heating_Weeks_jplMUR_Daily_MO06", 
#                    "DHW.CI95Max_Major_Degree_Heating_Weeks_jplMUR_Daily_MO06", 
#                    "DHW.CI95Max_Degree_Heating_Weeks_jplMUR_Daily_MO06",
#                    "DHW.Np10y_Degree_Heating_Weeks_jplMUR_Daily_MO06",             
#                    "DHW.Np10y_Major_Degree_Heating_Weeks_jplMUR_Daily_MO06")
# ))
save(eds_MO06, file = "eds_MO06_JPL_updated_DHW.Rdata")

######
merged2025_eds_PM_S_colony6m_DHW_all<-eds_MO06%>%
  left_join(south_ICRA_survey_data, by = "SITE")%>%
  filter(!is.na(PER_DEAD))
write.csv(merged2025_eds_PM_S_colony6m_DHW_all, file ="merged2025_eds_PM_S_colony6mUPDATEDJPL_DHW_all.csv") 
save(merged2025_eds_PM_S_colony6m_DHW_all, file="paper/merged2025_eds_PM_S_colony6mUPDATEDJPL_DHW_all.Rdata")
######

sub_numeric <- eds_MO06 %>%
  select(where(is.numeric))

#make a matrix
sub_numeric_matrix <- as.matrix(sub_numeric)
sub_numeric_matrix[!is.finite(sub_numeric_matrix)] <- NA
sub_numeric_matrix <- na.omit(sub_numeric_matrix)

#
M <- cor(sub_numeric_matrix, use = "pairwise.complete.obs") #pearsons
corrplot(M, tl.col="black", tl.cex = 0.5, type = 'upper') #correlation plot showing the correlation coefficient
res1 <- cor.mtest(sub_numeric_matrix, conf.level = 0.95)

png("plots/jpl_SST_correlations.png", width = 1800, height = 1600, res = 300)
corrplot(M, p.mat = res1$p, 
         sig.level = 0.05, 
         #insig = "p-value", # if you want to print nonsig pvalues
         order = 'hclust', 
         addrect = 2, 
         tl.srt = 45, 
         tl.cex = 0.6,  
         pch.cex = 0.8, 
         type = 'upper')

dev.off()
#combining correlogram with the significance test
#save
#png("plots/jpl_corrplot_output.png")


###############################################################################################
#subset variables you want to use:
sub_eds <- eds_MO06 %>%
  select(SITE,
         # SST_range = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_MO06,
         # SST_Mean = mean_Sea_Surface_Temperature_jplMUR_Daily_MO06
         DHW_Dur,
         DHW_Mean
  )

# range(sub_eds$SST_Mean)
# range(sub_eds$SST_range)
range(sub_eds$DHW_Dur)
range(sub_eds$DHW_Mean)

#next is merging variables of interest back with PM colony data
merged2025_eds_PM_S_colony6m_DHW<-sub_eds%>%
  left_join(south_ICRA_survey_data, by = "SITE")%>%
  filter(!is.na(PER_DEAD))
write.csv(merged2025_eds_PM_S_colony6m, file ="merged2025_eds_PM_S_colony6mUPDATEDJPL_DHW.csv") 
save(merged2025_eds_PM_S_colony6m, file="paper/merged2025_eds_PM_S_colony6mUPDATEDJPL_DHW.Rdata")

#check if SST mean and range are not correlated
sub_numeric <- sub_eds %>%
  select(where(is.numeric))

#make a matrix
sub_numeric_matrix <- as.matrix(sub_numeric)
sub_numeric_matrix[!is.finite(sub_numeric_matrix)] <- NA
sub_numeric_matrix <- na.omit(sub_numeric_matrix)

#correlation plot 
M <- cor(sub_numeric_matrix, use = "pairwise.complete.obs") #pearsons
res1 <- cor.mtest(sub_numeric_matrix, conf.level = 0.95)
corrplot(M, p.mat = res1$p, 
         sig.level = 0.05, 
         #insig = "p-value", # if you want to print nonsig pvalues
         order = 'hclust', 
         addrect = 2, 
         tl.srt = 45, 
         tl.cex = 0.6,  
         pch.cex = 0.8, 
         type = 'upper')


#################################################################################################################
#adjust 0s fo beta regression before averaging, then take mean PM, convert to proportion.

colnames(merged2025_eds_PM_S_colony6m_DHW)
n <- nrow(merged2025_eds_PM_S_colony6m_DHW)


site<-merged2025_eds_PM_S_colony6m_DHW%>%
  mutate(
    PER_DEAD_TRUE = (PER_DEAD * (n - 1) + 0.5) / n)%>%                       
  group_by(SITE, TAIL_BINS)%>%
  summarise(
    n = n(),
    mean_PM = mean(PER_DEAD_TRUE, na.rm = TRUE),
    prop_mean_PM = mean_PM / 100,
    DHW_Mean = first(DHW_Mean),
    DHW_Dur = first(DHW_Dur),
    Latitude= first(LATITUDE),
    LONGITUDE = first(LONGITUDE),
    .groups = "drop")

range(site$prop_mean_PM)
range(site$DHW_Mean)
range(site$DHW_Dur)

hist(site$prop_mean_PM, breaks = 30)

model<- glmmTMB(
  prop_mean_PM ~ (DHW_Mean * TAIL_BINS) + (DHW_Dur * TAIL_BINS),
  data = site,
  family = beta_family()
)
summary(model)

#Ferrari's R2
performance::r2(model)

############################################
#look at residuals. first simulate w DHARMa#
############################################

sim_res <- simulateResiduals(fittedModel = model, plot = TRUE)
#no significant deviations or problems detected
hist(sim_res$scaledResiduals, main = "Histogram of Scaled Residuals", xlab = "Residuals")

plotResiduals(sim_res, site$DHW_Mean)
plotResiduals(sim_res, site$DHW_Dur)

testUniformity(sim_res)     # Are residuals uniformly distributed?
#D = ns, KS test p = 0.53
testDispersion(sim_res)     # Is there overdispersion?
#p=0.528
testOutliers(sim_res)       # Are there extreme values?
#ns

#plot resudials by site
plotResiduals(sim_res, site$SITE)
#ns

################
#plot raw data##
################


site$Size_cat <- recode(site$TAIL_BINS,
                        "Q20" = "Small",
                        "QMED" = "Medium",
                        "Q80" = "Large")
#colors<-c("cyan4","purple3","gray67")
#colors<-c( "darkgrey", "#E1AD01",  "gold")
#colors<-c("grey", "orangered", "darkred" )
colors<-c("goldenrod", "gold", "darkgrey" )

site$Size_cat <- factor(site$Size_cat, levels = c("Small", "Medium", "Large"))

#plot
ggplot(site, aes(x = DHW_Mean, y = prop_mean_PM, color = Size_cat)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm",  se = TRUE) +
  #facet_wrap(~ Size_cat) +
  theme_bw() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(
    x = expression(bold("SST Mean ("*~degree*C*")")),
    y = expression(bold("Observed Partial Mortality")),
    # title = "Observed PM by DHW Mean Across Size Classes"
  ) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

ggplot(site, aes(x = DHW_Dur, y = prop_mean_PM, color = Size_cat)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm",  se = TRUE) +
  #facet_wrap(~ Size_cat) +
  theme_bw() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(
    x = expression(bold("SST Mean ("*~degree*C*")")),
    y = expression(bold("Observed Partial Mortality")),
    # title = "Observed PM by DHW Dur Across Size Classes"
  ) +
  theme(
    legend.position = "right",
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
small_data <- subset(site, TAIL_BINS == "Q20")
newdata_small<-small_data
newdata_small$DHW_Dur <- mean(small_data$DHW_Dur)  # Set SST_range to mean
newdata_small$DHW_Mean <- seq(min(small_data$DHW_Mean), 
                              max(small_data$DHW_Mean), 
                              length.out = nrow(newdata_small)) # making the sequence matches the number of rows. courtney used by = round(diff(range(small_data$SST_Mean)), 3) / nrow(small_data))

med_data <- subset(site, TAIL_BINS == "QMED")
newdata_med<-med_data
newdata_med$DHW_Dur <- mean(med_data$DHW_Dur)  # Set SST_range to mean
newdata_med$DHW_Mean <- seq(min(med_data$DHW_Mean),
                            max(med_data$DHW_Mean),
                            length.out = nrow(newdata_med))


large_data <- subset(site, TAIL_BINS == "Q80")
newdata_large<-large_data
newdata_large$DHW_Dur <- mean(large_data$DHW_Dur)  # Set SST_range to mean
newdata_large$DHW_Mean <- seq(min(large_data$DHW_Mean), 
                              max(large_data$DHW_Mean), 
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
all.newdata$Size_cat <- factor(all.newdata$Size_cat, levels = c("Large","Medium","Small"))
all.newdata<- all.newdata[order(all.newdata$Size_cat),];head(all.newdata)


#ver1
ggplot() +
  geom_line(data = all.newdata, aes(x = DHW_Mean, y = Predicted_PM, color = Size_cat), size = 1) +
  #geom_ribbon(data = all.newdata, aes(x = SST_Mean, ymin = Predict.lwr, ymax = Predict.upr), alpha = 0.1) +
  geom_ribbon(data = all.newdata, aes(x = DHW_Mean, ymin = Predict.lwr, ymax = Predict.upr, fill = Size_cat), alpha = 0.1) +
  #facet_wrap(~Size_cat)+
  geom_point(data = site, aes(x = DHW_Mean, y = prop_mean_PM, color = Size_cat), size = 2, alpha = 0.7) +
  #geom_rug(data = all.newdata, mapping = aes(x = SST_Mean, y = 0, color = Size_cat), sides = "b", size = 1.5)+
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.key.size = unit(0.7, 'cm'),
    legend.text = element_text(size = 14),
    text = element_text(size = 18),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  ylab(expression(bold("Predicted PM"))) +
  xlab(expression(bold("DHW Mean ("*~degree*C*")"))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)
)

ggsave("paper/DHW_mean_regression.png")


###############################################################

####Step 1: create 3 new data frames for time since heat stress interval (or size bin our case) and hold everything but variable of interest constant####
#set SST range to mean (holding it constant)
small_data <- subset(site, TAIL_BINS == "Q20")
newdata_small<-small_data
newdata_small$DHW_Mean <- mean(small_data$DHW_Mean)  # Set SST_range to mean
newdata_small$DHW_Dur <- seq(min(small_data$DHW_Dur), 
                              max(small_data$DHW_Dur), 
                              length.out = nrow(newdata_small)) # making the sequence matches the number of rows. courtney used by = round(diff(range(small_data$SST_Mean)), 3) / nrow(small_data))

med_data <- subset(site, TAIL_BINS == "QMED")
newdata_med<-med_data
newdata_med$DHW_Mean <- mean(med_data$DHW_Mean)  # Set SST_range to mean
newdata_med$DHW_Dur <- seq(min(med_data$DHW_Dur),
                            max(med_data$DHW_Dur),
                            length.out = nrow(newdata_med))


large_data <- subset(site, TAIL_BINS == "Q80")
newdata_large<-large_data
newdata_large$DHW_Mean <- mean(large_data$DHW_Mean)  # Set SST_range to mean
newdata_large$DHW_Dur <- seq(min(large_data$DHW_Dur), 
                              max(large_data$DHW_Dur), 
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
all.newdata$Size_cat <- factor(all.newdata$Size_cat, levels = c("Large","Medium","Small"))
all.newdata<- all.newdata[order(all.newdata$Size_cat),];head(all.newdata)


#ver1
ggplot() +
  geom_line(data = all.newdata, aes(x = DHW_Dur, y = Predicted_PM, color = Size_cat), size = 1) +
  #geom_ribbon(data = all.newdata, aes(x = SST_Mean, ymin = Predict.lwr, ymax = Predict.upr), alpha = 0.1) +
  geom_ribbon(data = all.newdata, aes(x = DHW_Dur, ymin = Predict.lwr, ymax = Predict.upr, fill = Size_cat), alpha = 0.1) +
  #facet_wrap(~Size_cat)+
  geom_point(data = site, aes(x = DHW_Dur, y = prop_mean_PM, color = Size_cat), size = 2, alpha = 0.7) +
  #geom_rug(data = all.newdata, mapping = aes(x = SST_Mean, y = 0, color = Size_cat), sides = "b", size = 1.5)+
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.key.size = unit(0.7, 'cm'),
    legend.text = element_text(size = 14),
    text = element_text(size = 18),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  ylab(expression(bold("Predicted PM"))) +
  xlab(expression(bold("DHW Dur"))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)


ggsave("paper/DHW_dur_regression.png")
