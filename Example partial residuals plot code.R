# Visualizing Interactions with Partial Residual Plots----------------------------------------
#This is sample code from my juvenile analysis.
#final.df = site-level data with reponse and predictor variables


####Step 1: create 3 new data frames for time since heat stress interval (or size bin our case) and hold everything but variable of interest constant####
#In the example below we are predicting juvenile density as a function of mean heat stress for each time since heat stress interval
#Hold everything but mean DHW constant at mean

#Heat Stress Severity x year since heat stress event
r <- subset(final.df,YearSinceDHW4<=3)
newdata1<-r
newdata1$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata1$scaled_CORAL <- mean(r$scaled_CORAL)
newdata1$scaled_CoralSec_A <- mean(r$scaled_CoralSec_A)
newdata1$scaled_SAND_RUB <- mean(r$scaled_SAND_RUB)
newdata1$scaled_EMA_MA <- mean(r$scaled_EMA_MA)
newdata1$scaled_HerbivoreBio <- mean(r$scaled_HerbivoreBio)
newdata1$scaled_WavePower <- mean(r$scaled_WavePower)
newdata1$scaled_Depth_Median<- mean(r$scaled_Depth_Median)
newdata1$scaled_logHumanDen <- mean(r$scaled_logHumanDen)
newdata1$scaled_MeanDHW10<-seq(min(r$scaled_MeanDHW10),max(r$scaled_MeanDHW10), #generate a sequence of values that is the same length of df r. You will need to fiddle with the value you are rounding (in this case I chose 3)
                               by=round(rg(r$scaled_MeanDHW10),3)/nrow(r))
newdata1$scaled_YearSinceDHW4<-mean(r$scaled_YearSinceDHW4)


m <- subset(final.df,YearSinceDHW4>3 & YearSinceDHW4 <=10)
newdata2<-m
newdata2$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata2$scaled_CORAL <- mean(m$scaled_CORAL)
newdata2$scaled_CoralSec_A <- mean(m$scaled_CoralSec_A)
newdata2$scaled_SAND_RUB <- mean(m$scaled_SAND_RUB)
newdata2$scaled_EMA_MA <- mean(m$scaled_EMA_MA)
newdata2$scaled_HerbivoreBio <- mean(m$scaled_HerbivoreBio)
newdata2$scaled_WavePower <- mean(m$scaled_WavePower)
newdata2$scaled_Depth_Median<- mean(m$scaled_Depth_Median)
newdata2$scaled_logHumanDen <- mean(m$scaled_logHumanDen)
newdata2$scaled_MeanDHW10<-seq(min(m$scaled_MeanDHW10),max(m$scaled_MeanDHW10),
                               by=round(rg(m$scaled_MeanDHW10),3)/nrow(m))
newdata2$scaled_YearSinceDHW4<-mean(m$scaled_YearSinceDHW4)


o <- subset(final.df,YearSinceDHW4>10)
newdata3<-o
newdata3$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata3$scaled_CORAL <- mean(o$scaled_CORAL)
newdata3$scaled_CoralSec_A <- mean(o$scaled_CoralSec_A)
newdata3$scaled_SAND_RUB <- mean(o$scaled_SAND_RUB)
newdata3$scaled_EMA_MA <- mean(o$scaled_EMA_MA)
newdata3$scaled_HerbivoreBio <- mean(o$scaled_HerbivoreBio)
newdata3$scaled_WavePower <- mean(o$scaled_WavePower)
newdata3$scaled_Depth_Median<- mean(o$scaled_Depth_Median)
newdata3$scaled_logHumanDen <- mean(o$scaled_logHumanDen)
newdata3$scaled_MeanDHW10<-seq(min(o$scaled_MeanDHW10),max(o$scaled_MeanDHW10),
                               by=round(rg(o$scaled_MeanDHW10),5)/nrow(o))
newdata3$scaled_YearSinceDHW4<-mean(o$scaled_YearSinceDHW4)

####Step 2: predict juvenile density as a function of DHW using the model output (e.g. best.mod in this case)####

p <- predict(best.mod, newdata = newdata1, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata1<-cbind(newdata1,p)
newdata1$Predict.lwr <- newdata1$Predicted_Juv - 1.96 * newdata1$SE_Juv # confidence interval upper bound
newdata1$Predict.upr <- newdata1$Predicted_Juv + 1.96 * newdata1$SE_Juv # confidence interval lower bound
newdata1$HSts_cat<-"0-3 years"

p <- predict(best.mod, newdata = newdata2, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata2<-cbind(newdata2,p)
newdata2$Predict.lwr <- newdata2$Predicted_Juv - 1.96 * newdata2$SE_Juv # confidence interval upper bound
newdata2$Predict.upr <- newdata2$Predicted_Juv + 1.96 * newdata2$SE_Juv # confidence interval lower bound
newdata2$HSts_cat<-"3-10 years"

p <- predict(best.mod, newdata = newdata3, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata3<-cbind(newdata3,p)
newdata3$Predict.lwr <- newdata3$Predicted_Juv - 1.96 * newdata3$SE_Juv # confidence interval upper bound
newdata3$Predict.upr <- newdata3$Predicted_Juv + 1.96 * newdata3$SE_Juv # confidence interval lower bound
newdata3$HSts_cat<-"> 10 years"

#Merge into 1 dataframe and add column for each HSts category. 
all.newdata<-rbind(newdata1,newdata2,newdata3)


#Unscaling predictor to plot on x axis -if you don't scale your predictors skip this step
att <- attributes(scale(final.df$MeanDHW10))
mylabels <- seq(0,14,2)
mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]

colors<-c("cyan4","purple3","gray67")

#Reorder HSts variables
all.newdata$HSts_cat <- factor(all.newdata$HSts_cat, levels = c("0-3 years","3-10 years","> 10 years"))
all.newdata<- all.newdata[order(all.newdata$HSts_cat),];head(all.newdata)



####Step 3: Plot parital residuals plot- note all 3 lines are included in the same plot- you may want to play with facetting####
plot1<-ggplot() +
  geom_line(data=all.newdata,aes(x = scaled_MeanDHW10, y = Predicted_Juv,color=HSts_cat),size=1) +
  geom_ribbon(data = all.newdata,aes(x = scaled_MeanDHW10,ymin = Predict.lwr, ymax = Predict.upr,fill=HSts_cat),alpha = 0.1)+
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key.size = unit(1.5, 'cm'),
    legend.text = element_text(size=16),
    text = element_text(size = 18),
    panel.grid = element_blank()
  ) +
  ylab(expression(bold(paste("Predicted Juvenile Colonies",m^-2)))) +
  xlab(expression(bold('Mean Max '^o*'C-weeks')))  +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)+
  scale_x_continuous(labels = comma(mylabels),breaks=mybreaks)+
  scale_y_continuous(limits=c(0,20))+
  geom_rug(data=final.df,mapping=aes(x=scaled_MeanDHW10,y=0))


plot1
