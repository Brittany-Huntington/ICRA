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
load("data/eds_output.Rdata")
load("data/ICRA_SIZE_PM_nofeb.RData")
load("data/ICRA_PM_S_site2025.RData") #site-level PM summary of 2025 south
load("data/ICRA_PM_S_colony2025.RData") #colony-level PM of 2025 south

ICRA_PM<- ICRA_SIZE_PM_nofeb %>%
  mutate(prop_DEAD = PER_DEAD / 100)

select = dplyr::select
rename  = dplyr::rename



#save the colnames as a file for ease in viewing variable names
column_names <- colnames(eds)
column_names_df <- data.frame(column_names)

#view just the 1 year variables
yr01_columns <- column_names[grepl("_YR01$", column_names)]
yr01_columns_df<- data.frame(yr01_columns)
non_zero_yr01_columns <- yr01_columns[colSums(eds[, yr01_columns] != 0) > 0]
print(non_zero_yr01_columns)

#subset variables you want to use:
sub_eds <- eds %>%
  select(SITE,
    lat,
     DHW_Mean = DHW.MeanMax_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    # DHW_MeanMax_Major = DHW.MeanMax_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
     DHW_Dur = DHW.MeanDur_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    # DHW_Dur_Major = DHW.MeanDur_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    # DHW_Max_Major = DHW.MaxMax_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    # DHW_Max_Major5 = DHW.MaxMax_Major_Degree_Heating_Weeks_jplMUR_Daily_YR10,
    # DHW_Mean_CRW = DHW.MeanMax_Degree_Heating_Weeks_CRW_Daily_YR01,
    # DHW_Mean_Major_CRW = DHW.MeanMax_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
    # DHW_Dur_CRW = DHW.MeanDur_Degree_Heating_Weeks_CRW_Daily_YR01,
    # DHW_Dur_Major_CRW = DHW.MeanDur_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
    # DHW_Max_Major_CRW = DHW.MaxMax_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
    # SST_AnnRange_CRW = mean_annual_range_Sea_Surface_Temperature_CRW_Daily_YR01,
    # SST_MonthRange_CRW = mean_monthly_range_Sea_Surface_Temperature_CRW_Daily_YR01,
    # SST_Mean_CRW = mean_Sea_Surface_Temperature_CRW_Daily_YR01,
    # SST_Q05_CRW = q05_Sea_Surface_Temperature_CRW_Daily_YR01,
    # SST_Q95_CRW = q95_Sea_Surface_Temperature_CRW_Daily_YR01,
    # SST_SD_CRW = sd_Sea_Surface_Temperature_CRW_Daily_YR01,
    # SST_BiweekRange_CRW = mean_biweekly_range_Sea_Surface_Temperature_CRW_Daily_YR01,
    SST_AnnRange = mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_MonthRange = mean_monthly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_Mean = mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_Q05 = q05_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_Q95 = q95_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_SD = sd_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_BiweekRange = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01
    
  )
write.csv(sub_eds, "edsparameters.csv")


sub_numeric <- sub_eds %>%
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


#significant_correlations <- cor_p_table[cor_p_table$P_Value < 0.05 & cor_p_table$Var1 != cor_p_table$Var2, ]
#write.csv(significant_correlations, "significant_correlations.csv", row.names = FALSE)

#Courtney did this:
#Testing for Multicolinarity
#preds<-r[,9:ncol(r)]
# library(GGally)
# ggpairs(preds)


#par(mfrow=c(1,1))
#M = cor(preds)
#png(width = 750, height = 750, filename = "T:/Benthic/Projects/Juvenile Project/Figures/Drivers/JuvenilePredictorsCorPlot_AllYears.png")
#corrplot(M, method = 'number')
#dev.off()


#######################################################################################
#make a df / csv of the variables you want to use in the analysis. EDIT THIS
use_sub<- sub_eds %>%
  select(SITE,
         #lat,
         DHW_Mean,
         #DHW_MeanMax_Major,
         DHW_Dur
         #DHW_Dur_Major,
         #DHW_Max_Major,
         #SST_AnnRange,
         #SST_MonthRange,
         #SST_Mean,
         #SST_Q05,
         #SST_Q95,
         #SST_SD,
         #SST_BiweekRange
         
  ) 


###########################################################################################################
#next is merging variables of interest back with Pm , density data.separate by size class for modeling.
#merge eds data with site-averaged PM for all 2025 south sites (march and feb)
merged2025_PM_S_site<-use_sub %>%
  left_join(ICRA_PM_S_site_2025, by = "SITE")%>%
  filter(!is.na(mean_PM))
write.csv(merged2025_PM_S_site, file ="merged2025_PM_S_site.csv")   

merged2025_PM_S_colony<-use_sub%>%
  left_join(ICRA_PM_S_colony_2025, by = "SITE")%>%
  filter(!is.na(PER_DEAD))
write.csv(merged2025_PM_S_colony, file ="merged2025_PM_S_colony.csv")  

# merge with PM data at colony level.
merged_PM_colony <- sub_eds %>%
  left_join(ICRA_PM, by = "SITE")%>%
  #select(-lon, -Area_surveyed_m2, -COLONYLENGTH, -LATITUDE, -LONGITUDE, MAX_DEPTH_M)%>%
  drop_na(PER_DEAD)

#subset by tailbin
small<- merged_PM_colony %>%
  filter (TAIL_BINS == "Q20")
write.csv(small, "small_ICRA_allpreds.csv") 

med<- merged_PM_colony %>%
  filter (TAIL_BINS == "QMED")
write.csv(med, "med_ICRA_allpreds.csv") 

large<- merged_PM_colony %>%
  filter (TAIL_BINS == "Q80")
write.csv(large, "large_ICRA_allpreds.csv") 

#define and scale predictors
preds <- use_sub %>%
  select(-SITE) %>%
  scale(center = TRUE, scale = TRUE)
colnames(preds)<-paste("scaled",colnames(preds),sep="_")

preds_df <- as.data.frame(preds)
preds_df$SITE <- use_sub$SITE

small.df <- small %>%
  left_join(preds_df, by = "SITE")

med.df <- med %>%
  left_join(preds_df, by = "SITE")

large.df <- large %>%
  left_join(preds_df, by = "SITE")

graphics.off()

