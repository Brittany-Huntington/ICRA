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
if(!require(ggeffects)){install.packages("ggeffects")}

rm(list=ls())
#dir = Sys.info()[7]
#setwd(paste0("C:/Users/", dir, "/Documents/github/ICRA/"))
setwd("C:/github/ICRA/data")


####LOAD RESPONSE DATA---------------

#load 2025 coral data 
icra<- read_csv("south_only_ICRA_Colony_level_data.csv")%>% mutate_if(is.character,as.factor) %>%
  filter(YEAR == "2025", !is.na(PER_DEAD))%>% droplevels()

plotNormalHistogram(icra$PER_DEAD) #potentially use >10% as a cutoff to look at prevalence of "severe partial mortality.

#transform colony level partial mortality to range from 0-1; recommended by Smithson & Verkuilen (2006)
icra <- icra %>% mutate(PER_DEAD = PER_DEAD/100)
n <- length(icra$PER_DEAD)  # number of observations
icra$PER_DEAD.adj <- (icra$PER_DEAD * (n - 1) + 0.5) / n
range(icra$PER_DEAD.adj)

####CREATE RESPONSE VARIABLES @ SITE LEVEL (site level means per size class)--------------
rv_size <- icra %>% filter(!is.na(TAIL_BINS)) %>%
  group_by(SITE, TAIL_BINS) %>%
  summarise(mean_PM = mean(PER_DEAD.adj, na.rm = TRUE), .groups = "drop")




####EXPLORE OTHER DRIVER VARIABLES (MAX HEAT AND VARITATION)-----------------

dat <- read_csv("C:/github/ICRA/merged_PM_site_all_YR01.csv")%>% mutate_if(is.character,as.factor)
colnames(dat)
dat <- dat[,-c(1:2, 69:77)]  #66 variables
dat <- dat[, sapply(dat, function(col) length(unique(col)) > 1)] #remove rows that only have one unique value; down to 49 variables
dat <- dat[, sapply(dat, is.numeric)]
dat <- dat %>% dplyr::select(contains("_jplMUR"))


#remove collinear variables
cor_matrix <- cor(dat, method = "pearson", use = "pairwise.complete.obs")
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")
colnames(dat)
dat.red <- dplyr::select(dat, DHW.MaxMax_Degree_Heating_Weeks_jplMUR_Daily_YR01 , mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
                         q95_Sea_Surface_Temperature_jplMUR_Daily_YR01, sd_Sea_Surface_Temperature_jplMUR_Daily_YR01, mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)

#remove more collinear variables
cor_matrix <- cor(dat.red, method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")
dat.red <- dplyr::select(dat.red, DHW.MaxMax_Degree_Heating_Weeks_jplMUR_Daily_YR01 , mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,
                         q95_Sea_Surface_Temperature_jplMUR_Daily_YR01, mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)

#remove more collinear variables
cor_matrix <- cor(dat.red, method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")
dat.red <- dplyr::select(dat.red, mean_Sea_Surface_Temperature_jplMUR_Daily_YR01, mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01)

#final driver variables--> rename metrics
dat.red <- dat.red %>% rename(SST_mean = mean_Sea_Surface_Temperature_jplMUR_Daily_YR01,SST_range = mean_biweekly_range_Sea_Surface_Temperature_jplMUR_Daily_YR01) 

sites <- read_csv("C:/github/ICRA/merged_PM_site_all_YR01.csv")%>% mutate_if(is.character,as.factor) %>% dplyr::select (SITE)
dat.red <- cbind(sites, dat.red)
rv_size <-  left_join(rv_size,dat.red)

levels(rv_size$TAIL_BINS)
levels(rv_size$TAIL_BINS) <- c("Small",  "Large", "Medium")
rv_size$TAIL_BINS <- factor(rv_size$TAIL_BINS,
                            levels = c("Small", "Medium", "Large"))

#####BETA REGRESSION--------------------
#Run B: build model with 2 fixed effects plus using size bin as an interactive effect----
# mean PM
glm.1 <- glmmTMB(mean_PM ~ SST_mean * TAIL_BINS + SST_range * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
summary(glm.1)


#Checking Model Diagnostics
sim_res <- simulateResiduals(fittedModel = glm.1, plot = TRUE)
testResiduals(sim_res)                 # Global tests
testDispersion(sim_res)               # Overdispersion
testZeroInflation(sim_res)            # Zero inflation
plotResiduals(sim_res, rv_size$SST_mean)  # Residuals vs predictor

plot(predict(glm.1, type = "response"), rv_size$mean_PM,
     xlab = "Predicted mean_PM", ylab = "Observed mean_PM")
abline(0, 1, col = "red")



####PARTIAL REGRESSION PLOTS--------------

#Option 1: Using ggeffects (easy & robust for interactions); 
# Get predicted values of mean_PM across the observed range of SST_mean for each level of TAIL_BINS, holding other variables constant (e.g., SST_range)
preds <- ggpredict(glm.1, terms = c("SST_mean", "TAIL_BINS"))

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
