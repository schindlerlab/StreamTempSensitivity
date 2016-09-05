setwd("C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt")
source("DynamicFactorAnalysis_TMB_UsingUnstructuredCorr_StdErr.R")

setwd("C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels")
wd1<-getwd()
output.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/Output/SnowpackPaperOutput/NEWEST/LAGS"
streams<-read.csv("WoodTogiakGeomorph.csv",header=T)
annual.temp.files.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/SnowpackPaperFiles"
annual.fils<-list.files(annual.temp.files.wd)

for(q in annual.fils[5])
{
  setwd(annual.temp.files.wd)
  #print(q)
  temp11<-read.csv(file = q, header = TRUE, stringsAsFactors = FALSE)

  
  stream.names<-names(temp11)[-c(1,2,3,4)]
  Zscore<-function(x){
    return((x-mean(x,na.rm=T))/sd(x,na.rm=T))
  }
  
  StreamTemps11<-temp11[,-c(1,2,3,4)]
  AirTemp11<-temp11[,3]
  
  SolRad11<-temp11[,4]
  Dates11<-as.Date(temp11[,1], format="%m/%d/%Y")
  DOY11<-temp11[,2]
  nday<-length(AirTemp11)
  
  Z_StreamTemps11<-t(apply(StreamTemps11,2,FUN=Zscore))
  Z_AirTemp11<-matrix(Zscore(AirTemp11),nrow=1)

  Z_SolRad11<-matrix(Zscore(SolRad11),nrow=1)

#   
#   st<-Sys.time()
#   mod11<-runDFA(Z_StreamTemps11,NumStates=1,ErrStruc='UNC',EstCovar=TRUE,Covars=rbind(Z_AirTemp11),EstSE=TRUE)
#   print("A")
# #   mod11_1lags<-runDFA(Z_StreamTemps11[,2:ncol(Z_StreamTemps11)],
# #                     NumStates=1,ErrStruc='UNC',EstCovar=TRUE,
# #                      Covars=rbind(Z_AirTemp11[2:(ncol(Z_StreamTemps11)-0)],Z_AirTemp11[1:(ncol(Z_StreamTemps11)-1)]))
#   difftime(Sys.time(),st)
#   print("B")
#  
#   tempsens1<-mod11$Estimates$D * (apply(StreamTemps11,2,sd,na.rm=T)/sd(AirTemp11))
# 
#   outtab1<-data.frame(stream.names,mod11$Estimates$Z,mod11$StdErr$Z,mod11$Estimates$D,mod11$StdErr$D,tempsens1)
#   colnames(outtab1)<-c("Stream","TrendLoad0","SDTL0","ATLoad0","SDAT0","TempSens0")
#   outtab1$Slope<-NA
#   outtab1$WS<-NA
#   outtab1$Elev<-NA
#   outtab1$Area<-NA
#   outtab1$Converge<-rep(mod11$Optimization$convergence,length(outtab1$Area))
#   outtab1$r2<-NA
#   outtab1$AIC<-rep(mod11$AIC,length(outtab1$r2))
#   for(k in 1:ncol(StreamTemps11))
#   {
#     
#     outtab1$r2[k]<-summary(lm(mod11$Fits[k,]~Z_StreamTemps11[k,]))$adj.r.squared
#     
#   }
#   
#   
#   for(i in 1:length(outtab1$Stream))
#   {
#     if(as.character(outtab1$Stream[i])%in%as.character(streams$Stream))
#     {
#       outtab1$Slope[i]<-streams$Slope[as.character(streams$Stream)==as.character(outtab1$Stream[i])]
#       outtab1$WS[i]<-streams$Watershed[as.character(streams$Stream)==as.character(outtab1$Stream[i])]
#       outtab1$Area[i]<-streams$Area[as.character(streams$Stream)==as.character(outtab1$Stream[i])]
#       outtab1$Elev[i]<-streams$Elev[as.character(streams$Stream)==as.character(outtab1$Stream[i])]
#     }
#     else{
#       outtab1$Slope[i]<-NA
#       outtab1$Area[i]<-NA
#       outtab1$Elev[i]<-NA
#       outtab1$WS[i]<-"Wood"
#     } 
#     
#   }
#   out.id<-substr(q,1,10)
#   out.yr<-substr(q,1,4)
#   outtab.name<-paste(out.id,"OutputTrendandCovariateLoadings_AirTemp_SnowPaper_NoLag.csv")
#   outtrend.name<-paste(out.id,"LatentTrend_SnowPaper_NoLag.csv")
#   outplot.name<-paste(out.id,"SummaryPlots_AirTemp_SnowPaper_NoLag.pdf")
#   
#   setwd(output.wd)
#   write.csv(t(mod11$Estimates$u),file=outtrend.name,row.names=F)
#   write.csv(outtab1,file=outtab.name,row.names=F)
#   
#   
#   
#   pdf(outplot.name,onefile=T)
#   #mod11$Estimates$u
#   plot(as.vector(mod11$Estimates$u),main=paste(out.yr,"Trend"))
#   #mod11$Estimates$Z # Trend loadings
#   barplot(t(mod11$Estimates$Z),names=colnames(StreamTemps11),las=2,main=paste(out.yr,"Trend Loadings"))
#   #mod11_1lags$Estimates$D # Temp sensitivities (z-score)
#   barplot(t(mod11$Estimates$D),names=colnames(StreamTemps11),las=2,main=paste(out.yr,"Air Temp Loadings"))
#   
#   plot(outtab1$Slope,outtab1$TempSens0,col=as.factor(outtab1$WS),pch=16,bty="l",ylab="Stream Temperature Sensitivity Air",
#        xlab="Slope",main=out.yr,xpd=F,yaxs="r")
#   if(out.yr!="2012") legend("topright",pch=16,col=c("black","red"),legend=c("Togiak","Wood"),bty="n")
#   else legend("topright",pch=16,col=c("green","black","red"),legend=c("Nushagak","Togiak","Wood"),bty="n")
#   lm1<-lm(outtab1$TempSens0~outtab1$Slope)
#   polygon(c(0,0,1000,1000),c(-1,0,0,-1),col=adjustcolor("grey",alpha.f=.2),border=F,xpd=F)
#   abline(lm1,lty=2,lwd=2,xpd=F)
#   mtext(side=3,text=summary(lm1)$adj.r.squared)
#   
#   for(i in 1:ncol(StreamTemps11))
#   {  
#     plot(Z_StreamTemps11[i,2:ncol(Z_StreamTemps11)],yaxs="r")
#     points(mod11$Fits[i,],type="l")
#     mtext(side=3,stream.names[i])
#   }
#   dev.off()
  
  print("Running 1 Lag Model")
  # One lag
  mod11_1lags<-runDFA(Z_StreamTemps11[,2:ncol(Z_StreamTemps11)],
                      NumStates=1,ErrStruc='UNC',EstCovar=TRUE,
                      Covars=rbind(Z_AirTemp11[2:(ncol(Z_StreamTemps11)-0)],Z_AirTemp11[1:(ncol(Z_StreamTemps11)-1)]),
                      ,EstSE=TRUE)
  print("Model Finished")
  tempsens1<-mod11_1lags$Estimates$D[,1] * (apply(StreamTemps11,2,sd,na.rm=T)/sd(AirTemp11))
  tempsens2<-mod11_1lags$Estimates$D[,2] * (apply(StreamTemps11,2,sd,na.rm=T)/sd(AirTemp11))
 
  outtab1<-data.frame(stream.names,mod11_1lags$Estimates$Z,mod11_1lags$StdErr$Z,
                      mod11_1lags$Estimates$D[,1],mod11_1lags$Estimates$D[,2],mod11_1lags$StdErr$D[,1],
                      mod11_1lags$StdErr$D[,2],tempsens1,tempsens2)
  colnames(outtab1)<-c("Stream","TrendLoad0","SDTL0","ATLoad0","ATLoad1","SDAT0","SDAT1","TempSens0","TempSens1")
  outtab1$Slope<-NA
  outtab1$WS<-NA
  outtab1$Elev<-NA
  outtab1$Area<-NA
  outtab1$Converge<-rep(mod11_1lags$Optimization$convergence,length(outtab1$Area))
  outtab1$r2<-NA
  outtab1$AIC<-rep(mod11_1lags$AIC,length(outtab1$r2))
  for(k in 1:ncol(StreamTemps11))
  {
    
    outtab1$r2[k]<-summary(lm(mod11_1lags$Fits[k,]~Z_StreamTemps11[k,2:ncol(Z_StreamTemps11)]))$adj.r.squared
    
  }

  
  for(i in 1:length(outtab1$Stream))
  {
    if(as.character(outtab1$Stream[i])%in%as.character(streams$Stream))
    {
      outtab1$Slope[i]<-streams$Slope[as.character(streams$Stream)==as.character(outtab1$Stream[i])]
      outtab1$WS[i]<-streams$Watershed[as.character(streams$Stream)==as.character(outtab1$Stream[i])]
      outtab1$Area[i]<-streams$Area[as.character(streams$Stream)==as.character(outtab1$Stream[i])]
      outtab1$Elev[i]<-streams$Elev[as.character(streams$Stream)==as.character(outtab1$Stream[i])]
    }
    else{
      outtab1$Slope[i]<-NA
      outtab1$Area[i]<-NA
      outtab1$Elev[i]<-NA
      outtab1$WS[i]<-"Wood"
    } 
    
  }
  out.id<-substr(q,1,10)
  out.yr<-substr(q,1,4)
  outtab.name<-paste(out.id,"OutputTrendandCovariateLoadings_AirTemp_SnowPaper_lag1.csv")
  outtrend.name<-paste(out.id,"LatentTrend_SnowPaper_lag1.csv")
  outplot.name<-paste(out.id,"SummaryPlots_AirTemp_SnowPaper_lag1.pdf")

  setwd(output.wd)
  write.csv(t(mod11_1lags$Estimates$u),file=outtrend.name,row.names=F)
  write.csv(outtab1,file=outtab.name,row.names=F)


  # Create summary figures in one pdf file
  pdf(outplot.name,onefile=T)
  plot(as.vector(mod11_1lags$Estimates$u),main=paste(out.yr,"Trend")) # Plot latent trend
  barplot(t(mod11_1lags$Estimates$Z),names=colnames(StreamTemps11),
          las=2,main=paste(out.yr,"Trend Loadings")) # Plot Trend Loadings
  barplot(t(mod11_1lags$Estimates$D[,1]),names=colnames(StreamTemps11),
          las=2,main=paste(out.yr,"Air Temp Loadings")) # Plot Covariate 1 loadings (Day of Temperature)
  barplot(t(mod11_1lags$Estimates$D[,2]),names=colnames(StreamTemps11),
          las=2,main=paste(out.yr,"Lag 1 Air Loadings")) # Plot Covariate 2 loadings (1 Day Lag Temperature)
  plot(outtab1$Slope,outtab1$TempSens0,col=as.factor(outtab1$WS),
       pch=16,bty="l",ylab="Stream Temperature Sensitivity Air",
       xlab="Slope",main=out.yr,xpd=F,yaxs="r") # Plot regression between watershed slope and Temperature sensitivity
  if(out.yr!="2012") legend("topright",pch=16,col=c("black","red"),
                            legend=c("Togiak","Wood"),bty="n")
  else legend("topright",pch=16,col=c("green","black","red"),legend=c("Nushagak","Togiak","Wood"),bty="n")
  lm1<-lm(outtab1$TempSens1~outtab1$Slope)
  polygon(c(0,0,1000,1000),c(-1,0,0,-1),col=adjustcolor("grey",alpha.f=.2),border=F,xpd=F)
  abline(lm1,lty=2,lwd=2,xpd=F)
  mtext(side=3,text=summary(lm1)$adj.r.squared)
  
  # Plot Model fits for each stream (one to a page)
  for(i in 1:ncol(StreamTemps11))
  {  
    plot(Z_StreamTemps11[i,2:ncol(Z_StreamTemps11)],yaxs="r") # Plot observed temps
    points(mod11_1lags$Fits[i,],type="l") # Plot model fits
    mtext(side=3,stream.names[i]) # label page
  }
  dev.off()
  
}


