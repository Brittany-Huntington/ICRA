#test which variables from eds output are correlated. Explore relationship w/ PM and environmental history.

rm(list = ls())
library(tidyverse)
library(corrplot)   
library(GGally)    
library(eds)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(readxl)
library(betareg)
library(purrr)
library(broom)
library(glmmTMB)
library(emmeans)
library(broom.mixed)
library(multcomp)
library(emmeans)

load("data/ICRA_SIZE_PM_nofeb.RData")
ICRA_PM<- ICRA_SIZE_PM_nofeb

load("data/eds_output.Rdata")

select = dplyr::select
rename  = dplyr::rename

#save the colnames as a file for ease in viewing variable names
column_names <- colnames(eds)
column_names_df <- data.frame(column_names)
#write.csv(column_names_df, "data/EDS_column_names.csv", row.names = FALSE)

#subset variables you want to use:
sub<- eds %>%
  select(year, lat, DHW.MeanMax_Degree_Heating_Weeks_CRW_Daily_YR01,
         DHW.MeanMax_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
         DHW.MeanDur_Degree_Heating_Weeks_CRW_Daily_YR01,
         #DHW.MeanDur_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
         DHW.YearsToLast_Major_Degree_Heating_Weeks_CRW_Daily_YR10,
         sd_Sea_Surface_Temperature_CRW_Daily_YR01,
         mean_annual_range_Sea_Surface_Temperature_CRW_Daily_YR01,
         mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         mean_monthly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         q05_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         q95_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         sd_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         #mean_Wave_Height_WW3_Global_Hourly_YR01,
         #mean_annual_range_Wave_Height_WW3_Global_Hourly_YR01,
         #mean_Wind_Speed_NCEI_Daily_YR01,
         #mean_annual_range_Wind_Speed_NCEI_Daily_YR01,
         mean_annual_range_KdPAR_NOAA_VIIRS_Monthly_YR01,
         mean_Chlorophyll_A_ESA_OC_CCI_v6.0_Monthly_YR01,
         mean_Bleaching_Hotspot_CRW_Daily_YR01,
         mean_annual_range_Bleaching_Hotspot_CRW_Daily_YR01
         
  ) %>%
  filter(year == 2025) #or use all years %>%


sub_numeric <- sub %>%
  select(where(is.numeric))

#remove incomplete pairwise cases. unsure if needed
#sub_numeric_clean <- sub_numeric[complete.cases(sub_numeric), ]
#remove year column
sub_numeric_clean<-sub_numeric%>%
  select(-year)
#make a matrix
sub_numeric_matrix <- as.matrix(sub_numeric_clean)
sub_numeric_matrix[!is.finite(sub_numeric_matrix)] <- NA
sub_numeric_matrix <- na.omit(sub_numeric_matrix)


M <- cor(sub_numeric_matrix, use = "pairwise.complete.obs") #pearsons
corrplot(M, tl.col="black", tl.cex = 0.5, type = 'upper') #correlation plot showing the correlation coefficient
res1 <- cor.mtest(sub_numeric_matrix, conf.level = 0.95)
#combining correlogram with the significance test
#save
png("plots/corrplot_output.png", width = 800, height = 800)
par(mar = c(10, 4, 4, 2)) 
# Generate the correlation plot
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

##########################################################################################
#now, visualize all variables that are significant;y correlated and remove from analysis
cor_matrix <- as.data.frame(as.table(M))
p_values <- as.data.frame(as.table(res1$p))

cor_p_table <- merge(cor_matrix, p_values, by = c("Var1", "Var2"))
names(cor_p_table) <- c("Var1", "Var2", "Correlation", "P_Value")

#write.csv(cor_p_table, "correlations.csv", row.names = FALSE)


significant_correlations <- cor_p_table[cor_p_table$P_Value < 0.05 & cor_p_table$Var1 != cor_p_table$Var2, ]
#write.csv(significant_correlations, "significant_correlations.csv", row.names = FALSE)

#######################################################################################
#make a df / csv of the variables you want to use in the analysis. EDIT THIS
sub<- eds %>%
  select(year, SITE, lat, lon, 
         #DHW.MeanMax_Degree_Heating_Weeks_CRW_Daily_YR01,
         #DHW.MeanMax_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
         DHW.MeanDur_Degree_Heating_Weeks_CRW_Daily_YR01,
         #DHW.MeanDur_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
         #DHW.YearsToLast_Major_Degree_Heating_Weeks_CRW_Daily_YR10,
         #sd_Sea_Surface_Temperature_CRW_Daily_YR01,
         #mean_annual_range_Sea_Surface_Temperature_CRW_Daily_YR01,
         #mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         mean_monthly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         #q05_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         #q95_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         sd_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
         #mean_Wave_Height_WW3_Global_Hourly_YR01,
         #mean_annual_range_Wave_Height_WW3_Global_Hourly_YR01,
         #mean_Wind_Speed_NCEI_Daily_YR01,
         #mean_annual_range_Wind_Speed_NCEI_Daily_YR01,
        ## mean_annual_range_KdPAR_NOAA_VIIRS_Monthly_YR01,
         #mean_Chlorophyll_A_ESA_OC_CCI_v6.0_Monthly_YR01,
         #mean_Bleaching_Hotspot_CRW_Daily_YR01,
         #mean_annual_range_Bleaching_Hotspot_CRW_Daily_YR01
         
  ) %>%
  filter(year == 2025) #or use all years %>%

#rename predictors
use_sub<- sub %>%
  rename(
    meandurDHW = DHW.MeanDur_Degree_Heating_Weeks_CRW_Daily_YR01,
    meanSST = mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    sdSST = sd_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    meanmonthlyrangeSST = mean_monthly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    meanbiweeklyrangeSST = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01
  )


#next is merging variables of interest back with Pm , density data.
#first merge with PM data at colony level.
merged_PM_colony <- use_sub %>%
  left_join(ICRA_PM, by = "SITE")%>%
  filter(year == 2025)%>% #or use all years %>%
  select(-year, -YEAR, -lon, -Area_surveyed_m2, COLONYLENGTH, -LATITUDE, -LONGITUDE, MAX_DEPTH_M)%>%
  drop_na(PER_DEAD)

#summarize PM per site
PM <- ICRA_PM %>%
  filter(year == 2025) %>%
  group_by(SITE) %>%
  summarise(
    n = sum(!is.na(PER_DEAD)),
    mean_PM = mean(PER_DEAD, na.rm = TRUE),
    sd_PM = sd(PER_DEAD, na.rm = TRUE),
    max_PM = max(PER_DEAD, na.rm = TRUE)
  ) %>%
  mutate(
    se_PM = sd_PM / sqrt(n),
    ci_lower = if_else(n > 1, mean_PM - qt(0.975, df = n - 1) * se_PM, NA_real_),
    ci_upper = if_else(n > 1, mean_PM + qt(0.975, df = n - 1) * se_PM, NA_real_)
  )
  #mutate(
  #  se_PM = sd_PM / sqrt(n),
  #  ci_lower = if_else(sd_PM > 0, mean_PM - qt(0.975, df = n - 1) * se_PM, NA_real_),
  #  ci_upper = if_else(sd_PM > 0, mean_PM + qt(0.975, df = n - 1) * se_PM, NA_real_))


#join with eds at site level. drop na's of PM and density
df <- use_sub %>%
  left_join(PM, by = "SITE") %>%
  mutate(
    mean_PM = if_else(is.nan(mean_PM), NA_real_, mean_PM)
  ) %>%
  drop_na(mean_PM)%>%
  filter(year == 2025) #or use all years 

##########################
#check for multicolinearity

#fr courtney:
#Testing for Multicolinarity
preds<-merged_PM_colony[,3:7]
# library(GGally)
# ggpairs(preds)


par(mfrow=c(1,1))
M = cor(preds)
#png(width = 750, height = 750, filename = "T:/Benthic/Projects/Juvenile Project/Figures/Drivers/JuvenilePredictorsCorPlot_AllYears.png")
corrplot(M, method = 'number')
dev.off()

#
#Confirmed with VIF - a priori cut off 3, but all less than 2.
#fit1 <- lm(JuvColDen ~ CORAL + CoralSec_A +  CCA +  EMA_MA + SAND_RUB + Depth_Median +  
 #            MeanDHW10 + Meanchla + MeanSST +
#             WavePower + YearSinceDHW4 + logHumanDen +HerbivoreBio, data = df.new)

fit1 <- lm(PER_DEAD ~ COLONYLENGTH + meandurDHW +  meanSST +  sdSST + meanmonthlyrangeSST + meanbiweeklyrangeSST,  
             data = merged_PM_colony)

car::vif(fit1)

#make sure VIF values <3. play around w dropping a variable. 

#remove any correlated vars and scale data. save w/ raw data and scaled. 
preds <- scale(preds, center = T, scale = T);colnames(preds)<-paste("scaled",colnames(preds),sep="_")

final.df<-cbind(merged_PM_colony,preds)


#plot top predictors
par(mfrow=c(2,2))
plot(final.df$PER_DEAD~final.df$meandurDHW)
plot(final.df$PER_DEAD~final.df$meanSST)
plot(final.df$PER_DEAD~final.df$sdSST)
plot(final.df$PER_DEAD~final.df$meanmonthlyrangeSST)
plot(final.df$PER_DEAD~final.df$meanbiweeklyrangeSST)

#now for backwards model selection: start w full model of all predictors,  remove least sig one at a time. wald test preditcts if regression coeff is sig diff fr 0.
#use drop1() for betareg. (????)Fill in

# Testing for polynomial relationships (this is to see whetehr non-libnear reelatuionships exist)

#Testing polynomial relationships with colonylength, the SST predictor vars. (depth here)
#can come back to this but probably dont need to do this given sample size 
d_poly3<-svyglm(JuvColCount ~  
                  poly(scaled_Depth_Median,3),
                design=des, family="poisson",offset=log(TRANSECTAREA_j))

d_poly2<-svyglm(JuvColCount ~  
                  poly(scaled_Depth_Median,2),
                design=des, family="poisson",offset=log(TRANSECTAREA_j))
d<-svyglm(JuvColCount ~  
            scaled_Depth_Median,
          design=des, family="poisson",offset=log(TRANSECTAREA_j))

anova(d,d_poly2) 
anova(d_poly3,d_poly2) 

AIC(d_poly3)
AIC(d_poly2)
AIC(d)
#2nd order polynomial is best fit


#######################################################################################################
#now need to model + run PCA/similar to see whether PM @ site level is correlated w/ any of these variables.


