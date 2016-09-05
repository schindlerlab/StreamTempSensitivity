output.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/Output/SnowpackPaperOutput/NEWEST/LAGS/Sept052016/SummaryFiles"
setwd(output.wd)
library(lme4)
dat<-read.csv("WoodRiverTempSensSnow_1lag.csv",header=T)
#dat<-dat[-c(147,134),]
dat$logArea<-log(dat$Area)
detach(dat)
attach(dat)



fit1<-lm(TempSens1~TempSens0*logArea)
fit2<-lm(TempSens1~TempSens0+logArea)
fit3<-lm(TempSens1~TempSens0)
fit4<-lm(TempSens1~Area)
aictab(cand.set=list(fit1,fit2,fit3,fit4))
summary(fit1)
hist(resid(fit1))
plot(TempSens1,fit1$fitted)
abline(0,1)

ts.pred <- seq(.0001, max(TempSens0), length.out = 50)
ar.pred <- log(seq(.1, 100, length.out = 50))
xy <- expand.grid(TempSens0 = ts.pred, 
                  logArea = ar.pred)

ts1.pred <- matrix (nrow = 50, ncol = 50, 
                   data = predict(fit1, newdata = data.frame(xy), 
                                  re.form=NA, type="response",
                                  allow.new.levels=T))

# predicted z-values, fitted points for droplines to surface
fitpoints <- predict(fit1) 
pdf("3DScatterPlot_LAG1TempSensitivity_WSArea_InstantTempSens_REmod.pdf",height=6,width=6)

scatter3D(z = TempSens1, x = TempSens0, y = exp(logArea), pch = 18, cex = 1, 
          theta = -50, phi = 20, ticktype = "detailed",
          xlab = "Lag 0 Temp Sensitivity", ylab = "Watershed Area", zlab = "Lag 1 Temperature Sensitivity",
          clab = "", bty="b2",
          surf = list(x = ts.pred, y = exp(ar.pred), z = ts1.pred, 
                      facets = NA, fit = fitpoints),
          colkey = list(length = .8, width = 0.6),          
          main = "Stream Temperature Sensitivity",xlim=c(0,max(TempSens0)),ylim=c(0,100),zlim=c(-.2,.5))

dev.off()

