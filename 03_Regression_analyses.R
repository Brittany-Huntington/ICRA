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
    DHW_Mean_CRW = DHW.MeanMax_Degree_Heating_Weeks_CRW_Daily_YR01,
    DHW_Mean_Major_CRW = DHW.MeanMax_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
    DHW_Dur_CRW = DHW.MeanDur_Degree_Heating_Weeks_CRW_Daily_YR01,
    DHW_Dur_Major_CRW = DHW.MeanDur_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
    DHW_Max_Major_CRW = DHW.MaxMax_Major_Degree_Heating_Weeks_CRW_Daily_YR01,
    SST_AnnRange_CRW = mean_annual_range_Sea_Surface_Temperature_CRW_Daily_YR01,
    SST_MonthRange_CRW = mean_monthly_range_Sea_Surface_Temperature_CRW_Daily_YR01,
    SST_Mean_CRW = mean_Sea_Surface_Temperature_CRW_Daily_YR01,
    SST_Q05_CRW = q05_Sea_Surface_Temperature_CRW_Daily_YR01,
    SST_Q95_CRW = q95_Sea_Surface_Temperature_CRW_Daily_YR01,
    SST_SD_CRW = sd_Sea_Surface_Temperature_CRW_Daily_YR01,
    SST_BiweekRange_CRW = mean_biweekly_range_Sea_Surface_Temperature_CRW_Daily_YR01,
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

png("plots/jpl_vs_CRW_correlations.png", width = 1800, height = 1600, res = 300)
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
png("plots/SST_comparison_corrplot_output.png")
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


#par(mfrow=c(1,1))
#M = cor(preds)
#png(width = 750, height = 750, filename = "T:/Benthic/Projects/Juvenile Project/Figures/Drivers/JuvenilePredictorsCorPlot_AllYears.png")
#corrplot(M, method = 'number')
#dev.off()


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
#next is merging variables of interest back with Pm , density data.separate by size class for modeling.
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

graphics.off()

#quick plots of predictors. 
par(mfrow=c(1,1))
plot(small$PER_DEAD~small$DHW_Mean)
plot(small$PER_DEAD~small$SST_AnnRange)
plot(small$PER_DEAD~small$SST_SD)

plot(med$PER_DEAD~med$DHW_Mean)
plot(med$PER_DEAD~med$SST_AnnRange)
plot(med$PER_DEAD~med$SST_SD)


plot(large$PER_DEAD~large$DHW_Mean)
plot(large$PER_DEAD~large$SST_AnnRange)
plot(large$PER_DEAD~large$SST_SD)

#predictors correspond to site and there are many response variables (coloniy % dead) per site... 
##########################
# look at jittered points#
##########################
predictors <- c("DHW_Mean", "SST_AnnRange", "SST_SD")

groups <- list(small = small, med = med, large = large)

par(mfrow = c(2, 3))

# Loop through each group and predictor to plot
for (group_name in names(groups)) {
  group_data <- groups[[group_name]]
  
  for (pred in predictors) {
    plot(jitter(group_data[[pred]], amount = 0.1), group_data$PER_DEAD,
         main = paste(group_name, ":", pred, "vs PER_DEAD"),
         xlab = paste(pred, "(jittered)"),
         ylab = "PER_DEAD")
  }
}


########################################
# Look at mean % dead at each predictor#
########################################
# Stack the data into long format
long_data <- lapply(predictors, function(var) {
  merged_PM_colony %>%
    select(PER_DEAD, TAIL_BINS, !!sym(var)) %>%
    rename(PredictorValue = !!sym(var)) %>%
    mutate(Predictor = var)
}) %>%
  bind_rows()

agg_data <- long_data %>%
  group_by(Predictor, PredictorValue, TAIL_BINS) %>%
  summarise(Mean_PER_DEAD = mean(PER_DEAD, na.rm = TRUE), .groups = "drop")

ggplot(agg_data, aes(x = PredictorValue, y = Mean_PER_DEAD)) +
  geom_line(aes(color = TAIL_BINS), size = 1) +
  #geom_smooth(aes(color = TAIL_BINS), method = "loess", se = FALSE)
  geom_point(aes(color = TAIL_BINS)) +
  facet_wrap(~ Predictor, scales = "free_x") +
  labs(x = "Predictor Value", y = "Mean % Dead Coral (PER_DEAD)",
       title = "PER_DEAD vs Predictor by Size Class") +
  theme_minimal()


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


######################################################################
##### Make this into a loop to plot  predictors and size classes######
######################################################################

predictors <- c("scaled_DHW_Mean", "scaled_SST_AnnRange", "scaled_SST_SD")

# List of data subsets
grouped_data <- list(Small = small.df, Medium = med.df, Large = large.df)

# Loop through each size class and each predictor
for (group_name in names(grouped_data)) {
  df <- grouped_data[[group_name]]
  
  # Filter only usable rows
  df <- df %>% filter(!is.na(prop_DEAD), prop_DEAD > 0 & prop_DEAD < 1)
  
  for (pred in predictors) {
    
    # Skip if predictor has all NA
    if (all(is.na(df[[pred]]))) next
    
    cat("\n\n-----", group_name, "-", pred, "-----\n")
    
    # Fit models
    form_lin <- as.formula(paste0("prop_DEAD ~ poly(", pred, ", 1, raw=TRUE)"))
    form_quad <- as.formula(paste0("prop_DEAD ~ poly(", pred, ", 2, raw=TRUE)"))
    form_cubic <- as.formula(paste0("prop_DEAD ~ poly(", pred, ", 3, raw=TRUE)"))
    
    try({
      d1 <- betareg(form_lin, data = df)
      d2 <- betareg(form_quad, data = df)
      d3 <- betareg(form_cubic, data = df)
      
      # Print model comparison
      print(lrtest(d1, d2, d3))
      cat("AIC - Linear:", AIC(d1), " Quadratic:", AIC(d2), " Cubic:", AIC(d3), "\n")
      
      # Generate new data for predictions
      x_seq <- seq(min(df[[pred]], na.rm = TRUE), max(df[[pred]], na.rm = TRUE), length.out = 100)
      newdata <- data.frame(x = x_seq)
      colnames(newdata) <- pred  # rename column to match formula
      
      newdata$pred_linear <- predict(d1, newdata, type = "response")
      newdata$pred_poly2  <- predict(d2, newdata, type = "response")
      newdata$pred_poly3  <- predict(d3, newdata, type = "response")
      
      # Plot
      p <- ggplot(df, aes_string(x = pred, y = "prop_DEAD")) +
        geom_point(alpha = 0.4) +
        geom_line(data = newdata, aes_string(x = pred, y = "pred_linear"), color = "blue", size = 1, linetype = "dashed") +
        geom_line(data = newdata, aes_string(x = pred, y = "pred_poly2"), color = "green", size = 1, linetype = "dotdash") +
        geom_line(data = newdata, aes_string(x = pred, y = "pred_poly3"), color = "red", size = 1) +
        labs(
          title = paste(group_name, "-", pred),
          x = pred,
          y = "Proportion Dead"
        ) +
        theme_minimal()
      
      print(p)
    }, silent = TRUE)  # continue loop if model fails
  }
}



###############################################################################
#summarize mean PM per site (not separating by size class)
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


