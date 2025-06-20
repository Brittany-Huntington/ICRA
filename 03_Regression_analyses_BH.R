#load libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(rcompanion)
library(corrplot)
if(!require(betareg)){install.packages("betareg")}
if(!require(clusterSim)){install.packages("clusterSim")} #data.Normalization function; centering and scaling data
if(!require(lmtest)){install.packages("lmtest")}
if(!require(glmmTMB)){install.packages("glmmTMB")}
if(!require(DHARMa)){install.packages("DHARMa")}


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




####LOAD DRIVER VARIABLES @ SITE LEVEL--------------
dat <- read_csv("C:/github/ICRA/merged2025_PM_S_site.csv")%>% mutate_if(is.character,as.factor) %>% dplyr::select(SITE: LONGITUDE)

plotNormalHistogram(dat$DHW_Mean)
plotNormalHistogram(dat$DHW_Dur)

#normalize drivers
#env <- dat %>% dplyr::select (DHW_Dur, DHW_Mean)
#env <- data.Normalization (as.matrix(env),type="n1",normalization="column")
#dat[, c("DHW_Dur", "DHW_Mean")] <- env

rv <- left_join(rv, dat)
rv_size <-  left_join(rv_size,dat)


####BETA REGRESSION--------------

#Run A: build model without size class across all 31 southern ICRA sites from 2025----
# mean PM
bm1 <- betareg(mean_PM ~ DHW_Mean + DHW_Dur, data = rv)
bmnull <- betareg(mean_PM ~ 1, data = rv)
lm1 <- lm(mean_PM ~ DHW_Mean + DHW_Dur, data = rv)

summary(bm1)
lrtest(bm1, bmnull)
AIC(bm1, bmnull, lm1) #betareg is better fit than linear model


#PM prevalence >10%
bm1 <- betareg(prev_PM_10 ~ DHW_Mean + DHW_Dur, data = rv)
bmnull <- betareg(prev_PM_10 ~ 1, data = rv)
lm1 <- lm(prev_PM_10 ~ DHW_Mean + DHW_Dur, data = rv)

summary(bm1)
lrtest(bm1, bmnull)
AIC(bm1, bmnull, lm1) #betareg is better fit than linear model


#PM prevalence >20%
bm1 <- betareg(prev_PM_20 ~ DHW_Mean + DHW_Dur, data = rv)
bmnull <- betareg(prev_PM_20 ~ 1, data = rv)
lm1 <- lm(prev_PM_20 ~ DHW_Mean + DHW_Dur, data = rv)

summary(bm1)
lrtest(bm1, bmnull)
AIC(bm1, bmnull, lm1) #betareg is better fit than linear model


#Run B: build model with 2 fixed effects plus using size bin as an interactive effect----

# mean PM
glm.1 <- glmmTMB(mean_PM ~ DHW_Mean * TAIL_BINS + DHW_Dur * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
summary(glm.1)


#PM prevalence >10%
glm.2 <- glmmTMB(prev_PM_10 ~ DHW_Mean * TAIL_BINS + DHW_Dur * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
summary(glm.2)


#PM prevalence >20%
glm.3 <- glmmTMB(prev_PM_20 ~ DHW_Mean * TAIL_BINS + DHW_Dur * TAIL_BINS, data = rv_size, family = beta_family(link = "logit"))
summary(glm.3)


#Run C: subsetting by size class first and then running beta regression----

rv_list <- split(rv_size, rv_size$TAIL_BINS) #subset dataframe by size bin


# mean PM
bm1 <- betareg(mean_PM ~ DHW_Mean + DHW_Dur, data = rv_list$Q20)
summary(bm1)

bm2 <- betareg(mean_PM ~ DHW_Mean + DHW_Dur, data = rv_list$QMED)
summary(bm2)

bm3 <- betareg(mean_PM ~ DHW_Mean + DHW_Dur, data = rv_list$Q80)
summary(bm3)


#PM prevalence >10%
bm1 <- betareg(prev_PM_10 ~ DHW_Mean + DHW_Dur, data = rv_list$Q20)
summary(bm1)

bm2 <- betareg(prev_PM_10 ~ DHW_Mean + DHW_Dur, data = rv_list$QMED)
summary(bm2)

bm3 <- betareg(prev_PM_10 ~ DHW_Mean + DHW_Dur, data = rv_list$Q80)
summary(bm3)


#PM prevalence >20%
bm1 <- betareg(prev_PM_20 ~ DHW_Mean + DHW_Dur, data = rv_list$Q20)
summary(bm1)

bm2 <- betareg(prev_PM_20 ~ DHW_Mean + DHW_Dur, data = rv_list$QMED)
summary(bm2)

bm3 <- betareg(prev_PM_20 ~ DHW_Mean + DHW_Dur, data = rv_list$Q80)
summary(bm3)


##Run D: colony level data using size as a continuous variable' limited to PM only as response----

#create dataframe for analysis
icra <- left_join(icra, dat) %>% mutate(PER_DEAD = PER_DEAD/100)

# mean PM
bm1 <- betareg(PER_DEAD ~ DHW_Mean + DHW_Dur + COLONYLENGTH, data = icra)



##Run E:  Beta regression using colony level; subsetted first (n = 11 sites)----
icra2 <- icra %>% filter(!is.na(TAIL_BINS))
icra_list <- split(icra2, icra2$TAIL_BINS) #subset dataframe by size bin

# mean PM
bm1 <- betareg(PER_DEAD ~ DHW_Mean + DHW_Dur + COLONYLENGTH, data = icra_list$Q20)
summary(bm1)

bm2 <- betareg(PER_DEAD ~ DHW_Mean + DHW_Dur + COLONYLENGTH, data = icra_list$QMED)
summary(bm2)

bm3 <- betareg(PER_DEAD ~ DHW_Mean + DHW_Dur+ COLONYLENGTH, data = rv_list$Q80)
summary(bm3)






####Checking Model Diagnostics-----------

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
  observedResponse = rv_list$Q20$mean_PM,
  fittedPredictedResponse = fitted(bm1)
)

# Plot diagnostics
plot(sim_res)
testDispersion(sim_res) #not super useful unless you have count or binomial data
testOutliers(sim_res)



