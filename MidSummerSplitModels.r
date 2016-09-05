setwd("C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt")
source("DynamicFactorAnalysis_TMB_UsingUnstructuredCorr.R")

setwd("C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels")
wd1<-getwd()
output.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/MidSumSplitOutput"
streams<-read.csv("WoodTogiakGeomorph.csv",header=T)
annual.temp.files.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/MidSummerSplit"
annual.fils<-list.files(annual.temp.files.wd)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


for(q in annual.fils)
{
  
  setwd(annual.temp.files.wd)
  print(q)
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
  #DOY11<-seq(153:250)
  #length(DOY11)
  
  Z_StreamTemps11<-t(apply(StreamTemps11,2,FUN=Zscore))
  Z_AirTemp11<-matrix(Zscore(AirTemp11),nrow=1)
  Z_SolRad11<-matrix(Zscore(SolRad11),nrow=1)
  #plot(Z_StreamTemps11[41,],type='l')
  #dim(Z_StreamTemps11)
  
  st<-Sys.time()
  mod12<-runDFA(Z_StreamTemps11,NumStates=1,ErrStruc='UNC',EstCovar=TRUE,Covars=Z_AirTemp11)
  difftime(Sys.time(),st)
  
  tempsens<-mod12$Estimates$D * (apply(StreamTemps11,2,sd,na.rm=T)/sd(AirTemp11))
  
  outtab<-data.frame(stream.names,mod12$Estimates$Z,mod12$Estimates$D,tempsens)
  colnames(outtab)<-c("Stream","TrendLoad","CovarLoad","TempSens")
  outtab$Slope<-NA
  outtab$WS<-NA
  outtab$Elev<-NA
  outtab$Area<-NA
  
  for(i in 1:length(outtab$Stream))
  {
    if(as.character(outtab$Stream[i])%in%as.character(streams$Stream))
    {
      outtab$Slope[i]<-streams$Slope[as.character(streams$Stream)==as.character(outtab$Stream[i])]
      outtab$WS[i]<-streams$Watershed[as.character(streams$Stream)==as.character(outtab$Stream[i])]
      outtab$Area[i]<-streams$Area[as.character(streams$Stream)==as.character(outtab$Stream[i])]
      outtab$Elev[i]<-streams$Elev[as.character(streams$Stream)==as.character(outtab$Stream[i])]
    }
    else{
      outtab$Slope[i]<-NA
      outtab$Area[i]<-NA
      outtab$Elev[i]<-NA
      outtab$WS[i]<-"Wood"
    } 
    
  }
  out.id<-paste(substr(q,1,10),substrRight(q,5))
  out.yr<-substr(q,1,4)
  outtab.name<-paste(out.id,"OutputTrendandCovariateLoadings_AirTemp_MidSummerSplit_SnowPackPaper.csv")
  outplot.name<-paste(out.id,"SummaryPlots_AirTemp_MidSummerSplit_SnowPackPaper.pdf")
  trendout.name<-paste(out.id,"SummaryTrends_AirTemp_MidSummerSplit_SnowPackPaper.csv")
  aictabname<-paste(out.id,"AIC.csv")
  mod.AIC2<-2*mod12$Optimization$objective+2*length(mod12$Optimization$par)
  setwd(output.wd)
  trend.out<-cbind(as.Date(temp11$Date,"%m/%d/%Y"),as.vector(mod12$Estimates$u),t(Z_AirTemp11),t(mod12$Fits))
  colnames(trend.out)<-c("Date","Trend","AirTemp",stream.names)
  write.csv(outtab,file=outtab.name,row.names=F)
  write.csv(trend.out,file=trendout.name,row.names=F)
  
  
}



