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


####LOAD DATA---------------

icra <- read_csv("C:/github/ICRA/merged2025_eds_PM_S_colony6mUPDATEDJPL_DHW_all.csv") %>% mutate_if(is.character, as.factor)

#transform colony level partial mortality to range from 0-1; recommended by Smithson & Verkuilen (2006)
n <- length(icra$PER_DEAD)  # number of observations
icra$PER_DEAD.adj <- (icra$PER_DEAD * (n - 1) + 0.5) / n
range(icra$PER_DEAD.adj)

####CREATE RESPONSE VARIABLES @ SITE LEVEL (site level means per size class)--------------
rv_size <- icra %>% 
  group_by(SITE, TAIL_BINS) %>%
  summarise(mean_PM = mean(PER_DEAD.adj, na.rm = TRUE), .groups = "drop")%>%
  mutate(mean_PM = mean_PM/100)

####EXPLORE OTHER DRIVER VARIABLES (MAX HEAT AND VARITATION)-----------------

dat <- read_csv("C:/github/ICRA/merged2025_eds_PM_S_colony6mUPDATEDJPL_DHW_all.csv") %>% mutate_if(is.character, as.factor) %>% dplyr::select(SITE, DHW_Mean:SST_BiweekRange)
dat <- dat %>%
  group_by(SITE)%>%
  summarise(across(everything(), mean, na.rm = TRUE))


#remove collinear variables
cor_matrix <- cor(dat[,2:14], method = "pearson", use = "pairwise.complete.obs")
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")
colnames(dat)
dat.red <- dplyr::select(dat, SITE, DHW_Mean, DHW_Dur, DHW_Dur_Major, SST_Mean, SST_SD, SST_BiweekRange, SST_AnnRange)

#remove more collinear variables
cor_matrix <- cor(dat.red[,2:8], method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")
colnames(dat.red)
dat.red <- dplyr::select(dat.red, -DHW_Dur_Major, -SST_SD, -SST_AnnRange)
cor_matrix <- cor(dat.red[,2:5], method = "pearson", use = "pairwise.complete.obs") 
corrplot.mixed(cor_matrix, upper = "color", lower = "number", diag = "n", tl.col = "black", tl.srt = 45, tl.pos = "lt")

sapply(dat.red[ , 2:5], range, na.rm = TRUE)

#rename and scale data: z-scored (mean = 0, sd = 1) 
dat.red <- dat.red %>% rename(SST_range = SST_BiweekRange, SST_mean = SST_Mean) 
#%>%  mutate(across(2:6, scale))


rv_size <- left_join(rv_size, dat.red)
levels(rv_size$TAIL_BINS)
levels(rv_size$TAIL_BINS) <- c("Small",  "Large", "Medium")
rv_size$TAIL_BINS <- factor(rv_size$TAIL_BINS,
                            levels = c("Small", "Medium", "Large"))




#####BETA REGRESSION--------------------
#Run B: build model with 2 fixed effects plus using size bin as an interactive effect----
# mean PM
glm.1 <- glmmTMB(mean_PM ~ SST_mean * TAIL_BINS + SST_range * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
glm.1a <- glmmTMB(mean_PM ~ DHW_Mean * TAIL_BINS + DHW_Dur * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
glm.1b <- glmmTMB(mean_PM ~ DHW_Dur * TAIL_BINS + SST_range * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
glm.1c <- glmmTMB(mean_PM ~ SST_mean * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
glm.1d <- glmmTMB(mean_PM ~ DHW_Mean * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))

summary(glm.1)
summary(glm.1a)
summary(glm.lb)
summary(glm.1c)

#model fit using AIC corrected for small sample sizes
MuMIn::AICc(glm.1, glm.1a, glm.1b, glm.1c, glm.1d)

#comparing one final check
glm.2 <- glmmTMB(mean_PM ~ DHW_Mean * TAIL_BINS + DHW_Dur * TAIL_BINS + SST_mean * TAIL_BINS, data = rv_size, family = beta_family(link = "logit") )
<<<<<<< HEAD
AIC(glm.1c, glm.2) #glm.1c still best model

summary(glm.1c)
=======
AIC(glm.1c, glm.2) #very similar
>>>>>>> 1b5a5060339de6fd48878fdb585623927c6ee9fc


#Checking Model Diagnostics for final model
sim_res <- simulateResiduals(fittedModel = glm.1c, plot = TRUE)
testResiduals(sim_res)                 # Global tests
testDispersion(sim_res)               # Overdispersion
testZeroInflation(sim_res)            # Zero inflation
plotResiduals(sim_res, rv_size$SST_mean)  # Residuals vs predictor

plot(predict(glm.1, type = "response"), rv_size$mean_PM,
     xlab = "Predicted mean_PM", ylab = "Observed mean_PM")
abline(0, 1, col = "red")

#check collinearity
#check_collinearity(glm.1c)


####PARTIAL REGRESSION PLOTS--------------

#Option 1: Using ggeffects (easy & robust for interactions); 
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
ggsave("C:/github/ICRA/plots/Fig.3_partial_mortality_plot_6mo.png", 
       width = 6, height = 5, dpi = 300, units = "in")
