#load libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(rcompanion)
library(corrplot)
library(car)
library(broom)
if(!require(betareg)){install.packages("betareg")}
if(!require(clusterSim)){install.packages("clusterSim")} #data.Normalization function; centering and scaling data
if(!require(lmtest)){install.packages("lmtest")}
if(!require(glmmTMB)){install.packages("glmmTMB")}
if(!require(DHARMa)){install.packages("DHARMa")}
if(!require(performance)){install.packages("performance")}

rm(list=ls())
#dir = Sys.info()[7]
#setwd(paste0("C:/Users/", dir, "/Documents/github/ICRA/"))
setwd("C:/github/ICRA/data")


####LOAD RESPONSE DATA---------------

#load 2025 coral data 
icra<- read_csv("south_only_ICRA_Colony_level_data.csv")%>% mutate_if(is.character,as.factor) %>%
  filter(YEAR == "2025", !is.na(PER_DEAD))%>% droplevels()

plotNormalHistogram(icra$PER_DEAD) #potentially use >10% as a cutoff to look at prevalence of "severe partial mortality.



####CREATE RESPONSE VARIABLES @ SITE LEVEL--------------

#site level means per size class
rv_size <- icra %>% filter(!is.na(TAIL_BINS)) %>%
  group_by(SITE, TAIL_BINS) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE), prev_PM_10 = round(mean(PER_DEAD > 10, na.rm = TRUE) , 1),
            prev_PM_20 = round(mean(PER_DEAD > 20, na.rm = TRUE) , 1), .groups = "drop")

cor_matrix <- rv_size %>% select(3:5) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_matrix, method = "circle") #mean and prevalence highly correlated

rv_size$mean_PM <- rv_size$mean_PM/100

#check ranges of response variables for 0 and 100
range(rv_size$mean_PM)
range(rv_size$prev_PM_10)
range(rv_size$prev_PM_20)

#adjust ranges to fall between 0-1
rv_size$mean_PM[rv_size$mean_PM == 0] <- rv_size$mean_PM[rv_size$mean_PM == 0] + 0.01
rv_size$mean_PM[rv_size$mean_PM == 1] <- rv_size$mean_PM[rv_size$mean_PM == 1] - 0.01
rv_size$prev_PM_10[rv_size$prev_PM_10 == 0] <- rv_size$prev_PM_10[rv_size$prev_PM_10 == 0] + 0.01
rv_size$prev_PM_10[rv_size$prev_PM_10 == 1] <- rv_size$prev_PM_10[rv_size$prev_PM_10 == 1] - 0.01
rv_size$prev_PM_20[rv_size$prev_PM_20 == 0] <- rv_size$prev_PM_20[rv_size$prev_PM_20 == 0] + 0.01
rv_size$prev_PM_20[rv_size$prev_PM_20 == 1] <- rv_size$prev_PM_20[rv_size$prev_PM_20 == 1] - 0.01


#site level means (irrespective of size class)
rv <- icra %>% group_by(SITE) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE), prev_PM_10 = round(mean(PER_DEAD > 10, na.rm = TRUE) , 1),
            prev_PM_20 = round(mean(PER_DEAD > 20, na.rm = TRUE) , 1), .groups = "drop")

cor_matrix <- rv %>% select(2:4) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_matrix, method = "circle") #mean and prevalence highly correlated

rv$mean_PM <- rv$mean_PM/100
range(rv$mean_PM)
range(rv$prev_PM_10)
range(rv$prev_PM_20)

#adjust ranges to fall between 0-1
rv$mean_PM[rv$mean_PM == 0] <- rv$mean_PM[rv$mean_PM == 0] + 0.01
rv$mean_PM[rv$mean_PM == 1] <- rv$mean_PM[rv$mean_PM == 1] - 0.01
rv$prev_PM_10[rv$prev_PM_10 == 0] <- rv$prev_PM_10[rv$prev_PM_10 == 0] + 0.01
rv$prev_PM_10[rv$prev_PM_10 == 1] <- rv$prev_PM_10[rv$prev_PM_10 == 1] - 0.01
rv$prev_PM_20[rv$prev_PM_20 == 0] <- rv$prev_PM_20[rv$prev_PM_20 == 0] + 0.01
rv$prev_PM_20[rv$prev_PM_20 == 1] <- rv$prev_PM_20[rv$prev_PM_20 == 1] - 0.01




####EXPLORE OTHER DRIVER VARIABLES (MAX HEAT AND VARITATION)-----------------

dat <- read_csv("C:/github/ICRA/merged_PM_site_all_YR01.csv")%>% mutate_if(is.character,as.factor)
colnames(dat)
dat <- dat[,-c(1:2, 69:77)]  #66 variables
dat <- dat[, sapply(dat, function(col) length(unique(col)) > 1)] #remove rows that only have one unique value; down to 49 variables
dat <- dat[, sapply(dat, is.numeric)]
dat <- dat %>% dplyr::select(contains("_jplMUR"))



#remove collinear variables
cor_matrix <- cor(dat, method = "pearson", use = "pairwise.complete.obs")
corrplot.mixed(cor_matrix, upper = "color",
               lower = "number",
               diag = "n",
               tl.col = "black",
               tl.srt = 45,
               tl.pos = "lt")
colnames(dat)
dat.red <- dplyr::select(dat, DHW.MaxMax_Degree_Heating_Weeks_jplMUR_Daily_YR01 , mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
                         q95_Sea_Surface_Temperature_jplMUR_Daily_YR01, sd_Sea_Surface_Temperature_jplMUR_Daily_YR01, mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)
cor_matrix <- cor(dat.red, method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color",
               lower = "number",
               diag = "n",
               tl.col = "black",
               tl.srt = 45,
               tl.pos = "lt")

dat.red <- dplyr::select(dat, DHW.MaxMax_Degree_Heating_Weeks_jplMUR_Daily_YR01 , mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
                         q95_Sea_Surface_Temperature_jplMUR_Daily_YR01, mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)
cor_matrix <- cor(dat.red, method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color",
               lower = "number",
               diag = "n",
               tl.col = "black",
               tl.srt = 45,
               tl.pos = "lt")

dat.red <- dplyr::select(dat, mean_Sea_Surface_Temperature_jplMUR_Daily_YR01, mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)

plotNormalHistogram(dat.red$DHW.MaxMax_Degree_Heating_Weeks_jplMUR_Daily_YR01)
plotNormalHistogram(dat.red$mean_Sea_Surface_Temperature_jplMUR_Daily_YR01)

dat.red <- dat.red %>%
  rename(
    SST_mean = mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
    SST_range = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01) 

sites <- read_csv("C:/github/ICRA/merged_PM_site_all_YR01.csv")%>% mutate_if(is.character,as.factor) %>% dplyr::select (SITE)
dat.red <- cbind(sites, dat.red)
rv <- left_join(rv, dat.red)
rv_size <-  left_join(rv_size,dat.red)






#Run A: build model without size class across all 31 southern ICRA sites from 2025----
# mean PM
bm1 <- betareg(mean_PM ~ SST_mean + SST_range, data = rv)
bmnull <- betareg(mean_PM ~ 1, data = rv)
summary(bm1)
lrtest(bm1, bmnull)
AIC(bm1, bmnull) #betareg is better fit than linear model


#PM prevalence >10%
bm1 <- betareg(prev_PM_10 ~ SST_mean + SST_range, data = rv)
bmnull <- betareg(prev_PM_10 ~ 1, data = rv)
summary(bm1)
lrtest(bm1, bmnull)
AIC(bm1, bmnull, lm1) #betareg is better fit than linear model


#PM prevalence >20%
bm1 <- betareg(prev_PM_20 ~ SST_mean + SST_range, data = rv)
bmnull <- betareg(prev_PM_20 ~ 1, data = rv)
summary(bm1)
lrtest(bm1, bmnull)
AIC(bm1, bmnull, lm1) #betareg is better fit than linear model




#Run B: build model with 2 fixed effects plus using size bin as an interactive effect----

# mean PM
glm.1 <- glmmTMB(mean_PM ~ SST_mean * TAIL_BINS + SST_range * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
summary(glm.1)


#PM prevalence >10%
glm.2 <- glmmTMB(prev_PM_10 ~ SST_mean * TAIL_BINS + SST_range * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
summary(glm.2)


#PM prevalence >20%
glm.3 <- glmmTMB(prev_PM_20 ~ SST_mean * TAIL_BINS + SST_range * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
summary(glm.3)




#Run C: subsetting by size class first and then running beta regression----
rv_list <- split(rv_size, rv_size$TAIL_BINS) #subset dataframe by size bin

# mean PM
bm1 <- betareg(mean_PM ~ SST_mean + SST_range, data = rv_list$Q20)
summary(bm1)

bm2 <- betareg(mean_PM ~ SST_mean + SST_range, data = rv_list$QMED)
summary(bm2)

bm3 <- betareg(mean_PM ~ SST_mean + SST_range, data = rv_list$Q80)
summary(bm3)


#PM prevalence >10%
bm1 <- betareg(prev_PM_10 ~ SST_mean + SST_range, data = rv_list$Q20)
summary(bm1)

bm2 <- betareg(prev_PM_10 ~ SST_mean + SST_range, data = rv_list$QMED)
summary(bm2)

bm3 <- betareg(prev_PM_10 ~ SST_mean + SST_range, data = rv_list$Q80)
summary(bm3)


#PM prevalence >20%
bm1 <- betareg(prev_PM_20 ~ SST_mean + SST_range, data = rv_list$Q20)
summary(bm1)

bm2 <- betareg(prev_PM_20 ~ SST_mean + SST_range, data = rv_list$QMED)
summary(bm2)

bm3 <- betareg(prev_PM_20 ~ SST_mean + SST_range, data = rv_list$Q80)
summary(bm3)



##Run D: colony level data using size as a continuous variable' limited to PM only as response----

#create dataframe for analysis
icra <- icra %>% filter(!is.na(COLONYLENGTH))
n <- nrow(icra)
icra$PER_DEAD_adj <- (icra$PER_DEAD * (n - 1) + 0.5) / n
icra <- left_join(icra, dat.red) %>% mutate(PER_DEAD_adj = PER_DEAD_adj/100)

# mean PM
bm1 <- betareg(PER_DEAD_adj ~ SST_mean + SST_range + COLONYLENGTH, data = icra)
summary(bm1)



####Checking Model Diagnostics-----------
check_collinearity(bm1)

# Plot residuals vs. fitted values
plot(bm1, which = 1)  # like base R lm plotting

# QQ plot of residuals
plot(bm1, which = 2)

# Simulate new responses manually from the fitted beta distribution
simulateFunction <- function(fittedModel, nsim) {
  mu <- fitted(fittedModel)
  phi <- fittedModel$coefficients$precision
  shape1 <- mu * phi
  shape2 <- (1 - mu) * phi
  replicate(nsim, rbeta(length(mu), shape1, shape2))
}

# Create DHARMa object for your given model using the custom simulator
sim_res <- createDHARMa(
  simulatedResponse = simulateFunction(bm1, 250),
  observedResponse = icra$PER_DEAD_adj,
  fittedPredictedResponse = fitted(bm1)
)

# Plot diagnostics
plot(sim_res)
testDispersion(sim_res) #not super useful unless you have count or binomial data
testOutliers(sim_res)


#####PLOT------------


# Step 1: Fit a linear model per TAIL_BINs group and extract R²
stats_labels <- rv_size %>%
  group_by(TAIL_BINS) %>%
  do({
    mod <- lm(mean_PM ~ SST_mean, data = .)
    tidy_mod <- glance(mod)
    tibble(
      r2 = tidy_mod$r.squared,
      pval = tidy_mod$p.value
    )
  }) %>%
  mutate(label = paste0("R² = ", round(r2, 2), 
                        ", p = ", signif(pval, 2)))

rv_size$TAIL_BINS <- factor(rv_size$TAIL_BINS, levels = c("Q20", "QMED", "Q80"))

ggplot(rv_size, aes(x = SST_mean, y = mean_PM)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ TAIL_BINS) +
  geom_text(data = stats_labels,
            aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.5,
            inherit.aes = FALSE, size = 4) +
  theme_minimal() +
  labs(
    x = "SST Mean (daily)",
    y = "Mean Partial Mortality (%)",
    title = "Relationship between SST Mean and Partial Mortality by Size Class"
  )
