output.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/Output/SnowpackPaperOutput/NEWEST/LAGS/Sept052016/SummaryFiles"
setwd(output.wd)
library(lme4)
dat<-read.csv("WoodRiverTempSensSnow_1lag_SameStreams.csv",header=T)
dat<-read.csv("WoodRiverTempSensSnow_1lag.csv",header=T)
#dat<-dat[dat$Stream!="Rainbow"&dat$Stream!="Cottonwood",]
#dat<-dat[-c(147,134),]
#dat<-dat[-c(50,56),]
dat$logSlope<-log(dat$Slope)
dat$logRelSnow<-log(dat$RelSnow)
detach(dat)
attach(dat)
## =======================================================================
## scatter3D with fitted surface : the mtcars dataset
## implemented by Karline Soetaert
## =======================================================================

require(plot3D)
library(AICcmodavg)
# linear fit
zfit <- lm(TempSens0 ~ scale(logSlope,center=T)+scale(RelSnow,center=T))
fit <- lmer(TempSens0 ~ logSlope*RelSnow+(1|Stream),REML=F)
fit2 <- lmer(TempSens0 ~ logSlope+logRelSnow+(1|Stream),REML=F)
fit3 <- lmer(TempSens0 ~ logSlope*logRelSnow+(1|Stream),REML=F)

fit4<-lmer(TempSens0 ~ Slope*RelSnow+(1|Stream),REML=F)
fit5<-lmer(TempSens0 ~ Slope+RelSnow+(1|Stream),REML=F)
fit6<-lmer(TempSens0 ~ logSlope+RelSnow+(1|Stream),REML=F)
fit7<-lmer(TempSens0 ~ Slope*logRelSnow+(1|Stream),REML=F)
fit8<-lmer(TempSens0 ~ Slope+logRelSnow+(1|Stream),REML=F)
fit9<-lmer(TempSens0 ~ Slope+(1|Stream),REML=F)
fit10<-lmer(TempSens0 ~ RelSnow+(1|Stream),REML=F)
fit11<-lmer(TempSens0 ~ logRelSnow+(1|Stream),REML=F)
fit12<-lmer(TempSens0 ~ logSlope+(1|Stream),REML=F)

aictab(cand.set=list(fit,fit2,fit3,fit4,fit5,
                  fit6,fit7,fit8,fit9,fit10,
                  fit11,fit12))
fit14<-lm(TempSens0 ~ logSlope+RelSnow)
AIC(fit,fit2,fit3,fit4, fit5)

AICc(fit)
AICc(fit2)
AICc(fit3)
AICc(fit4)
AICc(fit5)
AICc(fit6)
AICc(fit7)
AICc(fit8)
AICc(fit9)
AICc(fit10)
AICc(fit11)
AICc(fit12)

#fit2<-loess(TempSens ~ Elev*RelSnow)
# predict on x-y grid, for surface
sl.pred <- log(seq(1, max(Slope), length.out = 30))
rs.pred <- seq(.4, 1, length.out = 30)
xy <- expand.grid(logSlope = sl.pred, 
                  RelSnow = rs.pred)

ts.pred <- matrix (nrow = 30, ncol = 30, 
                    data = predict(fit14, newdata = data.frame(xy), 
                                                re.form=NA, type="response",
                                                allow.new.levels=T))

# predicted z-values, fitted points for droplines to surface
fitpoints <- predict(fit14) 
pdf("3DScatterPlot_TempSensitivity_Slope_Snow_1Lag_REmod.pdf",height=6,width=6)

scatter3D(z = TempSens0, x = Slope, y = RelSnow, pch = 18, cex = 1, 
          theta = 130, phi = 20, ticktype = "detailed",
          xlab = "Slope", ylab = "Snow Index", zlab = "Temperature Sensitivity",
          clab = "", bty="b2",
          surf = list(x = exp(sl.pred), y = rs.pred, z = ts.pred, 
                      facets = NA, fit = fitpoints),
          colkey = list(length = .8, width = 0.6),          
          main = "Stream Temperature Sensitivity",xlim=c(0,max(exp(logSlope))),ylim=c(.4,1),zlim=c(0,.6))


dev.off()
detach(mtcars)

library(rjags)
library(runjags)
library(R2jags)
setwd(output.wd)
model.dat<-read.csv("WoodRivTempSensOUTPUTDATA.csv",header=T)
model.dat$logSlope<-log(dat$Slope)
model.dat$logRelSnow<-log(dat$RelSnow)
detach(model.dat)
attach(model.dat)
n<-length(Year)
Nyr<-length(unique(Year))
nstream<-length(unique(streamnum))
jags.params<-c("pred","alpha","cee","siga","sigc","mua","muc","beta","mub","sigb")
jags.data<-list("nobs" = n,"lslope" = logSlope,"lSnow"=logRelSnow, "ts" = TempSens,
                "nstream"=nstream,"stream"=streamnum)
#jags.inits<-function(){list(modex=runif(Nyr,0,4),sdx=runif(Nyr,0,3),r=rnorm(Nyr,10,2))}
setwd("C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels")
model.location <- paste("StreamTempSnowModel_logSlopelogSnow.txt",sep="")

# Set MCMC parameters
mcmcchains <- 3
mcmcthin <- 10
mcmcburn <- 100000
samples2save <- 10000

# Run model
testModel<- jags(jags.data, inits =NULL, parameters.to.save = jags.params,
                 model.file=model.location,n.chains=mcmcchains,
                 n.thin=mcmcthin, n.burnin=mcmcburn, n.iter=(mcmcburn+samples2save),DIC=TRUE)

attach.jags(testModel)
modelout2<-list(Summary=testModel$BUGSoutput$summary,pD=testModel$BUGSoutput$pD,DIC=testModel$BUGSoutput$DIC)
modelout<-testModel$BUGSoutput$sims.matrix
sims<-as.mcmc(testModel)
#str(modelout)
plot(testModel)
#setwd("C:/Users/larogers-admin/My Documents/Walsworth.Data/Chignik Data/Coho Model Output")      # Set working directory to storage on hard drive
# setwd("C:/Users/tewals/Documents")
write.table(modelout2,"Linear_logSlope_logSnow_randomInterceptANDSnow.csv",append=TRUE)

McmcList <- vector("list", length=dim(modelout$sims.array)[2])
for(i in 1:length(McmcList))
{
  McmcList[[i]] = as.mcmc(modelout$sims.array[,i,])
}

Mcmcdat<-rbind(McmcList[[1]],McmcList[[2]],McmcList[[3]])
write.csv(Mcmcdat,file="Linear_logSlope_logSnow_randomInterceptANDSnowOUTDATA.csv",row.names=FALSE)



setwd("C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels")
Mcmcdat<-read.csv("Linear_logSlope_logSnow_randomInterceptANDSnowOUTDATA.csv")
library(RColorBrewer)
output.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/Output"
setwd(output.wd)
dat<-read.csv("WoodRivTempSensOUTPUTDATA.csv",header=T)
dat$logSlope<-log(dat$Slope)
dat$logRelSnow<-log(dat$RelSnow)

slopes<-aggregate(dat$Elev,list("Stream"=dat$Stream),max)
slopes$Rank<-rank(slopes$x,ties.method="first")
nstr<-max(slopes$Rank)
dat$color<-NA
for(i in 1:length(dat[,1]))
{
  strm<-dat$Stream[i]
  dat$color[i]<-slopes$Rank[slopes$Stream==strm]
  
}


colors<-colorRampPalette(c("darkorange","dodgerblue"))(nstr)
plot(colMeans(Mcmcdat[,c(2:47)]),colMeans(Mcmcdat[,c(48:93)]),
     xlab="Snowpack Sensitivity",
     ylab="Intercept",type="n",
     bty="l")
text(colMeans(Mcmcdat[,c(2:47)]),colMeans(Mcmcdat[,c(48:93)]),labels=slopes$Stream,
     col=colors[dat$color])
#head(Mcmcdat)

slopes2<-seq(0,30,by=1)
preds<-matrix(NA,nrow=length(slopes2),ncol=5)
for(i in 1:5)
{
  preds[,i]<-median(Mcmcdat[,1+i])+median(Mcmcdat[,1])*slopes2
}
pdf("StreamTempSensSlope_RandomEffectsModel_LinearYearInt.pdf",width=9,height=7)
transparency<-1
cols<-colorRampPalette(c("green","blue","purple"))(5)
plot(slopes2,preds[,1],type="l",ylim=c(-.3,.8),col=cols[1],ylab="Stream Temperature Sensitivity",
     xlab="Watershed Slope",bty="l")
points(Slope[Yr==1],TempSens[Yr==1],pch=16,col=adjustcolor(cols[1],alpha.f=transparency))

for(j in 2:5)
{
  lines(slopes2,preds[,j],col=cols[j])
  points(Slope[Yr==j],TempSens[Yr==j],col=adjustcolor(cols[j],alpha.f=transparency),pch=16)
  
}
abline(h=0,lty=2)
legend("topright",lty=1,pch=16,col=cols,legend=seq(2011,2015),bty="n")
dev.off()

cees<-apply(Mcmcdat[,2:6],2,median)
bees<-apply(Mcmcdat[,6:10],2,median)
ays<-apply(Mcmcdat[,1:5],2,median)
plot(unique(RelSnow),cees)
plot(unique(RelSnow),bees,col="red")
plot(unique(RelSnow),ays,col="blue")
