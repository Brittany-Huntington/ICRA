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
ICRA_PM<- ICRA_SIZE_PM_nofeb %>%
  mutate(prop_DEAD = PER_DEAD / 100)

select = dplyr::select
rename  = dplyr::rename



#save the colnames as a file for ease in viewing variable names
column_names <- colnames(eds)
column_names_df <- data.frame(column_names)

#view just the 1 year variables
yr01_columns <- column_names[grepl("_YR01$", column_names)]
non_zero_yr01_columns <- yr01_columns[colSums(eds[, yr01_columns] != 0) > 0]
print(non_zero_yr01_columns)

#subset variables you want to use:
sub <- eds %>%
  select(SITE,
    lat,
    DHW_Mean = DHW.MeanMax_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    DHW_Mean_Major = DHW.MeanMax_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    DHW_Dur = DHW.MeanDur_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    DHW_Dur_Major = DHW.MeanDur_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    DHW_Max_Major = DHW.MaxMax_Major_Degree_Heating_Weeks_jplMUR_Daily_YR01,
    SST_AnnRange = mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_MonthRange = mean_monthly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_Mean = mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_Q05 = q05_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_Q95 = q95_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_SD = sd_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_BiweekRange = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01
  )



sub_numeric <- sub %>%
  select(where(is.numeric))

#make a matrix
sub_numeric_matrix <- as.matrix(sub_numeric)
sub_numeric_matrix[!is.finite(sub_numeric_matrix)] <- NA
sub_numeric_matrix <- na.omit(sub_numeric_matrix)

#
M <- cor(sub_numeric_matrix, use = "pairwise.complete.obs") #pearsons
corrplot(M, tl.col="black", tl.cex = 0.5, type = 'upper') #correlation plot showing the correlation coefficient
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

dev.off()
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

write.csv(cor_p_table, "correlations.csv", row.names = FALSE)


significant_correlations <- cor_p_table[cor_p_table$P_Value < 0.05 & cor_p_table$Var1 != cor_p_table$Var2, ]
write.csv(significant_correlations, "significant_correlations.csv", row.names = FALSE)

#Courtney did this:
#Testing for Multicolinarity
#preds<-r[,9:ncol(r)]
# library(GGally)
# ggpairs(preds)


par(mfrow=c(1,1))
M = cor(preds)
png(width = 750, height = 750, filename = "T:/Benthic/Projects/Juvenile Project/Figures/Drivers/JuvenilePredictorsCorPlot_AllYears.png")
corrplot(M, method = 'number')
dev.off()


#######################################################################################
#make a df / csv of the variables you want to use in the analysis. EDIT THIS
use_sub<- sub %>%
  select(SITE,
         #lat,
         DHW_Mean,
         #DHW_Mean_Major,
         #DHW_Dur,
         #DHW_Dur_Major,
         #DHW_Max_Major,
         SST_AnnRange,
         #SST_MonthRange,
         #SST_Mean,
         #SST_Q05,
         #SST_Q95,
         SST_SD,
         #SST_BiweekRange
         
  ) 


###########################################################################################################
#next is merging variables of interest back with Pm , density data.
#first merge with PM data at colony level.
merged_PM_colony <- use_sub %>%
  left_join(ICRA_PM, by = "SITE")%>%
  #select(-lon, -Area_surveyed_m2, -COLONYLENGTH, -LATITUDE, -LONGITUDE, MAX_DEPTH_M)%>%
  drop_na(PER_DEAD)

#subset by tailbin
small<- merged_PM_colony %>%
  filter (TAIL_BINS == "Q20")

med<- merged_PM_colony %>%
  filter (TAIL_BINS == "QMED")

large<- merged_PM_colony %>%
  filter (TAIL_BINS == "Q80")


#define and scale preds
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

#quick plots of predictors
par(mfrow=c(2,2))
plot(small$PER_DEAD~small$DHW_Mean)
plot(small$PER_DEAD~small$SST_AnnRange)
plot(small$PER_DEAD~small$SST_SD)

plot(med$PER_DEAD~med$DHW_Mean)
plot(med$PER_DEAD~med$SST_AnnRange)
plot(med$PER_DEAD~med$SST_SD)


plot(large$PER_DEAD~large$DHW_Mean)
plot(large$PER_DEAD~large$SST_AnnRange)
plot(large$PER_DEAD~large$SST_SD)

#if here is nonlinearity, test polynomial fit
#d: Linear effect of depth
#d_poly2: Quadratic relationship (e.g., hump-shaped)
#d_poly3: Cubic relationship (allows for more bends in the curve)
###############
##small#######
###############
d_poly3 <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, 3, raw = TRUE), 
                     data = small.df)

d_poly2 <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, 2, raw = TRUE), 
                     data = small.df)
d <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, raw = TRUE), 
                        data = small.df)
lrtest(d, d_poly2, d_poly3) 

AIC(d_poly3)
AIC(d_poly2)
AIC(d)
# 3 (polynomial) fits better for small size class

newdata <- data.frame(
  scaled_DHW_Mean = seq(min(small.df$scaled_DHW_Mean, na.rm=TRUE),
                        max(small.df$scaled_DHW_Mean, na.rm=TRUE),
                        length.out = 100))

newdata$pred_linear <- predict(d, newdata, type = "response")
newdata$pred_poly2  <- predict(d_poly2, newdata, type = "response")
newdata$pred_poly3  <- predict(d_poly3, newdata, type = "response")

ggplot(small.df, aes(x = scaled_DHW_Mean, y = prop_DEAD)) +
  geom_point(alpha = 0.4) +  # observed data points
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_linear), color = "blue", size = 1, linetype = "dashed") +
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_poly2), color = "green", size = 1, linetype = "dotdash") +
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_poly3), color = "red", size = 1) +
  labs(
    x = "Scaled DHW Mean",
    y = "Proportion Dead",
    title = "Model fits: Linear (blue), Quadratic (green), Cubic (red)"
  ) +
  theme_minimal()

###############
##### med #####
###############
d_poly3 <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, 3, raw = TRUE), 
                   data = med.df)

d_poly2 <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, 2, raw = TRUE), 
                   data = med.df)
d <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, raw = TRUE), 
             data = med.df)
lrtest(d, d_poly2, d_poly3) 

AIC(d_poly3)
AIC(d_poly2)
AIC(d)
# 3 (polynomial) fits better for med size class

newdata <- data.frame(
  scaled_DHW_Mean = seq(min(med.df$scaled_DHW_Mean, na.rm=TRUE),
                        max(med.df$scaled_DHW_Mean, na.rm=TRUE),
                        length.out = 100))

newdata$pred_linear <- predict(d, newdata, type = "response")
newdata$pred_poly2  <- predict(d_poly2, newdata, type = "response")
newdata$pred_poly3  <- predict(d_poly3, newdata, type = "response")

ggplot(med.df, aes(x = scaled_DHW_Mean, y = prop_DEAD)) +
  geom_point(alpha = 0.4) +  # observed data points
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_linear), color = "blue", size = 1, linetype = "dashed") +
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_poly2), color = "green", size = 1, linetype = "dotdash") +
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_poly3), color = "red", size = 1) +
  labs(
    x = "Scaled DHW Mean",
    y = "Proportion Dead",
    title = "Model fits: Linear (blue), Quadratic (green), Cubic (red)"
  ) +
  theme_minimal()

###############
#### large ####
###############
d_poly3 <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, 3, raw = TRUE), 
                   data = large.df)

d_poly2 <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, 2, raw = TRUE), 
                   data = large.df)
d <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, raw = TRUE), 
             data = large.df)
lrtest(d, d_poly2, d_poly3) 

AIC(d_poly3)
AIC(d_poly2)
AIC(d)
# linear polynomial is best for large class

# Create a sequence covering the range of your predictor
newdata <- data.frame(
  scaled_DHW_Mean = seq(min(large.df$scaled_DHW_Mean, na.rm=TRUE),
                        max(large.df$scaled_DHW_Mean, na.rm=TRUE),
                        length.out = 100))

newdata$pred_linear <- predict(d, newdata, type = "response")
newdata$pred_poly2  <- predict(d_poly2, newdata, type = "response")
newdata$pred_poly3  <- predict(d_poly3, newdata, type = "response")

ggplot(large.df, aes(x = scaled_DHW_Mean, y = prop_DEAD)) +
  geom_point(alpha = 0.4) +  # observed data points
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_linear), color = "blue", size = 1, linetype = "dashed") +
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_poly2), color = "green", size = 1, linetype = "dotdash") +
  geom_line(data = newdata, aes(x = scaled_DHW_Mean, y = pred_poly3), color = "red", size = 1) +
  labs(
    x = "Scaled DHW Mean",
    y = "Proportion Dead",
    title = "Model fits: Linear (blue), Quadratic (green), Cubic (red)"
  ) +
  theme_minimal()

#PLOT SMALL
att <- attributes(scale(small.df$DHW_Mean))

# Set breaks in original units (adjust as fits your variable's range)
mylabels <- seq(9, 19, 1)  # change 0 and 40 to min/max range of DHW_Mean

#make breaks 
mybreaks <- scale(mylabels, center = att$`scaled:center`, scale = att$`scaled:scale`)[,1]

# model
mod <- betareg(prop_DEAD ~ poly(scaled_DHW_Mean, 3, raw = TRUE), data = small.df)

#predict data. Here Courtney calcualted SE but we can't do that w beta regression...
p <- predict(d, newdata = df.d, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata<-cbind(df.d,p)
newdata$Predict.lwr <- newdata$Predicted_Juv - 1.96 * newdata$SE_Juv # confidence interval upper bound
newdata$Predict.upr <- newdata$Predicted_Juv + 1.96 * newdata$SE_Juv # confidence interval lower bound
head(newdata)


att <- attributes(scale(final.df$HerbivoreBio))
mylabels <- seq(0,95,10)
mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]

#Plot
ggplot(newdata, aes(x = scaled_HerbivoreBio, y = Predicted_Juv)) +
  geom_line() +
  geom_ribbon(data = newdata,
              aes(ymin = Predict.lwr, ymax = Predict.upr),
              alpha = 0.1)+
  geom_rug(data=newdata,mapping=aes(x=scaled_HerbivoreBio,y=0,color=REGION))+
  ylab("Predicted Juvenile Abudance") +
  xlab("Herbivore Biomass (g/m2)")+ 
  scale_x_continuous(labels=mylabels,breaks=mybreaks)




#Global model by size class


#courtneys
final.df$Strat_conc<-paste(final.df$OBS_YEAR, final.df$REGION,final.df$ISLAND,final.df$STRATANAME,sep = "_") #dont know if i need this
des<-svydesign(id=~1, strata=~ Strat_conc, weights=~sw,data=final.df) #dont know if i need this
global.mod1<-glm(PER_DEAD ~
                      poly(scaled_CORAL,3,raw=TRUE)*scaled_MeanDHW10+ 
                      scaled_CCA*poly(scaled_Depth_Median,2,raw=TRUE)+
                      scaled_CoralSec_A*scaled_MeanDHW10 +
                      scaled_EMA_MA*scaled_MeanDHW10 +
                      scaled_SAND_RUB*scaled_MeanDHW10 +
                      scaled_HerbivoreBio*scaled_MeanDHW10 +
                      poly(scaled_Depth_Median,2,raw=TRUE)*scaled_MeanDHW10 +
                      scaled_Meanchla +
                      scaled_MeanSST +
                      scaled_WavePower*scaled_MeanDHW10+
                      scaled_YearSinceDHW4*scaled_MeanDHW10+
                      scaled_logHumanDen*scaled_MeanDHW10,
                    design=des, family="beta",offset=log(TRANSECTAREA_j))

###############################################################################
#summarize PM per site 
PM_by_site <- ICRA_PM %>%
  group_by(SITE) %>%
  summarise(
    n = sum(!is.na(PER_DEAD)),
    mean_PM = mean(PER_DEAD, na.rm = TRUE),
    sd_PM = sd(PER_DEAD, na.rm = TRUE),
    max_PM = max(PER_DEAD, na.rm = TRUE)
  ) %>%
  mutate(
    se_PM = sd_PM / sqrt(n),
    ci_lower = if_else(sd_PM > 0, mean_PM - qt(0.975, df = n - 1) * se_PM, NA_real_),
    ci_upper = if_else(sd_PM > 0, mean_PM + qt(0.975, df = n - 1) * se_PM, NA_real_))


