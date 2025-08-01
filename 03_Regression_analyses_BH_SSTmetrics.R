#load libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(rcompanion)
library(corrplot)
library(car)
library(broom)
library(MuMIn)
library(emmeans)
if(!require(betareg)){install.packages("betareg")}
if(!require(clusterSim)){install.packages("clusterSim")} #data.Normalization function; centering and scaling data
if(!require(lmtest)){install.packages("lmtest")}
if(!require(glmmTMB)){install.packages("glmmTMB")}
if(!require(DHARMa)){install.packages("DHARMa")}
if(!require(performance)){install.packages("performance")}
if(!require(ggeffects)){install.packages("ggeffects")}


rm(list=ls())
#dir = Sys.info()[7]
#setwd(paste0("C:/Users/", dir, "/Documents/github/ICRA/"))
setwd("C:/github/ICRA/data")


####LOAD RESPONSE DATA---------------

#load 2025 coral data 
icra<- read_csv("south_only_ICRA_Colony_level_data.csv")%>% mutate_if(is.character,as.factor) %>%
  filter(YEAR == "2025", !is.na(PER_DEAD), !is.na(TAIL_BINS))%>% droplevels()

plotNormalHistogram(icra$PER_DEAD) #potentially use >10% as a cutoff to look at prevalence of "severe partial mortality.

#transform colony level partial mortality to range from 0-1; recommended by Smithson & Verkuilen (2006)
icra <- icra %>% mutate(PER_DEAD = PER_DEAD/100)
n <- length(icra$PER_DEAD)  # number of observations
icra$PER_DEAD.adj <- (icra$PER_DEAD * (n - 1) + 0.5) / n
range(icra$PER_DEAD.adj)

####CREATE RESPONSE VARIABLES @ SITE LEVEL (site level means per size class)--------------
rv_size <- icra %>% 
  group_by(SITE, TAIL_BINS) %>%
  summarise(mean_PM = mean(PER_DEAD.adj, na.rm = TRUE), .groups = "drop")

range(rv_size$mean_PM)


####EXPLORE OTHER DRIVER VARIABLES (MAX HEAT AND VARITATION)-----------------

dat <- read_csv("C:/github/ICRA/merged_PM_site_all_YR01.csv")%>% mutate_if(is.character,as.factor) %>% dplyr::select(contains("_jplMUR"))
colnames(dat)
dat <- dat[, sapply(dat, function(col) length(unique(col)) > 1)] #remove rows that only have one unique value; down to 15 variables
dat <- dat[, sapply(dat, is.numeric)]



#explore colinearity
cor_matrix <- cor(dat, method = "pearson", use = "pairwise.complete.obs")
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")
colnames(dat)
#removed variables colinear with DHW_Mean or SST_Mean (pearsons r <0.55)
dat.red <- dplyr::select(dat, DHW.MeanMax_Degree_Heating_Weeks_jplMUR_Daily_YR01 , DHW.MeanDur_Degree_Heating_Weeks_jplMUR_Daily_YR01, mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
                         q05_Sea_Surface_Temperature_jplMUR_Daily_YR01:mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)

#remove more collinear variables DHW_Dur (pearsons r <0.55)
cor_matrix <- cor(dat.red, method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")
dat.red <- dplyr::select(dat.red, -q95_Sea_Surface_Temperature_jplMUR_Daily_YR01,-q05_Sea_Surface_Temperature_jplMUR_Daily_YR01,-mean_annual_range_Sea_Surface_Temperature_jplMUR_Daily_YR01, -mean_monthly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)

#remove more collinear variables with range metrics
cor_matrix <- cor(dat.red, method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")
dat.red <- dplyr::select(dat.red, -sd_Sea_Surface_Temperature_jplMUR_Daily_YR01)

cor_matrix <- cor(dat.red, method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")



#final driver variables...all pearsonʻs r <0.55--> rename metrics
dat.red <- dat.red %>% rename(SST_mean = mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
                              SST_range = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01, 
                              DHW_mean = DHW.MeanMax_Degree_Heating_Weeks_jplMUR_Daily_YR01,
                              DHW_dur = DHW.MeanDur_Degree_Heating_Weeks_jplMUR_Daily_YR01)
#%>%  mutate(across(2:5, scale)) #z-score transformation optional

sites <- read_csv("C:/github/ICRA/merged_PM_site_all_YR01.csv")%>% mutate_if(is.character,as.factor) %>% dplyr::select (SITE)
dat.red <- cbind(sites, dat.red)
rv_size <-  left_join(rv_size,dat.red)

levels(rv_size$TAIL_BINS)
levels(rv_size$TAIL_BINS) <- c("Small",  "Large", "Medium")
rv_size$TAIL_BINS <- factor(rv_size$TAIL_BINS,
                            levels = c("Small", "Medium", "Large"))


#####BETA REGRESSION--------------------
glm.1 <- glmmTMB(mean_PM ~ SST_mean * TAIL_BINS + SST_range * TAIL_BINS, data = rv_size, family = beta_family(link = "logit")) #SST metrics only

#explore other models:
glm.1a <- glmmTMB(mean_PM ~ DHW_mean * TAIL_BINS + DHW_dur * TAIL_BINS, data = rv_size, family = beta_family(link = "logit")) #DHW metrics rather than SST
glm.1b <- glmmTMB(mean_PM ~ SST_mean * TAIL_BINS + DHW_dur * TAIL_BINS + SST_range * TAIL_BINS, data = rv_size, family = beta_family(link = "logit")) #three factor model with severity, duration, and variation
glm.1c <- glmmTMB(mean_PM ~ SST_mean * TAIL_BINS, data = rv_size, family = beta_family(link = "logit")) #single factor models of just intensity
glm.1d <- glmmTMB(mean_PM ~ DHW_mean * TAIL_BINS, data = rv_size, family = beta_family(link = "logit")) #single factor models of just intensity

 
# Create a model selection table, using AIC corrected for small sample sizes
model.sel(glm.1, glm.1a, glm.1b, glm.1c, glm.1d, rank = "AICc")
summary(glm.1c) #favored model based on AICc
performance::r2(glm.1c) #Ferrari & Cribari-Neto’s pseudo R^2


#Checking Model Diagnostics
sim_res <- simulateResiduals(fittedModel = glm.1c, plot = TRUE)
testResiduals(sim_res)                 # Global tests
testDispersion(sim_res)               # Overdispersion
testOutliers(sim_res)                 # test outliers
testUniformity(sim_res)               # test uniformity of the residuals

plotResiduals(sim_res, rv_size$SST_mean)  # Residuals vs predictor
plotResiduals(sim_res, predict(glm.1c, type = "response")) ##catch issues in model fit not explained by SST_mean alone

plot(predict(glm.1, type = "response"), rv_size$mean_PM,
     xlab = "Predicted mean_PM", ylab = "Observed mean_PM")
abline(0, 1, col = "red")



# Get trends (slopes) of SST_mean for each TAIL_BINS level
trends <- emtrends(glm.1c, specs = "TAIL_BINS", var = "SST_mean")
summary(trends, infer = c(TRUE, TRUE))

#test whether the slope differences between bins are significant
pairs(trends)



####PARTIAL REGRESSION PLOTS--------------

# Get predicted values of mean_PM across the observed range of SST_mean for each level of TAIL_BINS, holding other variables constant (e.g., SST_range)
preds <- ggpredict(glm.1c, terms = c("SST_mean", "TAIL_BINS"))

# Plot
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  geom_point(data = rv_size, aes(x = SST_mean, y = mean_PM, color = TAIL_BINS), alpha = 0.5, size = 2) +
  scale_color_viridis_d(name = "Colony Size", option = "D") +
  scale_fill_viridis_d(name = "Colony Size", option = "D") +
  labs(
    x = "SST mean (°C)",
    y = "Predicted Partial Mortality (proportion)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )
ggsave("C:/github/ICRA/plots/Fig.3_partial_mortality_plot.png", 
       width = 6, height = 5, dpi = 300, units = "in")