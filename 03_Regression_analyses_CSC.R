rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
library(dplyr)
library(DHARMa)
library(betareg)

dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/ICRA/"))


#LOAD DATA
sm<-read.csv("small_ICRA_avg.csv"); sm<-sm[,2:ncol(sm)]
med<-read.csv("med_ICRA_avg.csv"); med<-med[,2:ncol(med)]
lg<-read.csv("large_ICRA_avg.csv"); lg<-lg[,2:ncol(lg)]

sm.final<-sm%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur))

#small has 3 sites with 0 PM in small corals. make the zeros 0.5
sm.final <- sm.final %>%
  mutate(prop_mean_PM_adj = (prop_mean_PM * (n - 1) + 0.05) / n)

med.final<-med%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur))

lg.final<-lg%>%
  mutate(prop_mean_PM = mean_PM / 100,
         scaled_DHW_Mean = scale(DHW_Mean),
         scaled_DHW_Dur = scale(DHW_Dur))


# Quick plots of predictors -------------------------------------------
par(mfrow=c(2,2))
plot(lg.final$mean_PM~lg.final$DHW_Mean)
plot(lg.final$mean_PM~lg.final$DHW_Dur)

plot(med.final$mean_PM~med.final$DHW_Mean)
plot(med.final$mean_PM~med.final$DHW_Dur)

##### Model Selection ---assuming normal distribution ####
#Large colonies- no transformation needed
lg.mod<-lm(mean_PM~scaled_DHW_Mean+scaled_DHW_Dur,data=lg.final)

#model diagnostics
lg.diag <- simulateResiduals(fittedModel = lg.mod)
plot(lg.diag) #looks good

#Medium colonies - sqrt transform needed
med.mod<-lm(sqrt(mean_PM)~scaled_DHW_Mean+scaled_DHW_Dur,data=med.final)

#model diagnostics
med.diag <- simulateResiduals(fittedModel = med.mod)
plot(med.diag) #looks good

#Small colonies - sqrt transform needed
sm.mod<-lm(sqrt(mean_PM)~scaled_DHW_Mean+scaled_DHW_Dur,data=sm.final)

#model diagnostics
sm.diag <- simulateResiduals(fittedModel = sm.mod)
plot(sm.diag) #looks good

#generate summaries
summary(sm.mod)
summary(med.mod)
summary(lg.mod)

#Backwards model selection lg colonies
RED.MOD1 <- update(sm.mod, .~. -scaled_DHW_Dur) #drop term
anova(lg.mod, RED.MOD1,method="Wald") #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD1)



##### Model Selection ---beta regressions ####
sm_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = sm.final)
summary(sm_model)

med_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = med.final)
summary(med_model)

large_model <- betareg(prop_mean_PM ~ scaled_DHW_Mean + scaled_DHW_Dur, data = lg.final)
summary(large_model)

#