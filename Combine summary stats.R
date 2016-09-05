#==============================================================
# Create a summary file for Stream Temperature DFA models
#   - File appends each year's output file into a single file
#==============================================================
all.input.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/Output/SnowpackPaperOutput/NEWEST/LAGS/Sept052016/LoadingsOutput"
#same.input.wd<-"C:/Users/larogers-admin/Documents/WaterTemperatureSnowmelt/fwdplotsandmodels/Output/SnowpackPaperOutput/NEWEST/LAGS/Aug102016/SameStreams/LoadingsOutput"

# Create data frame to append relative snowpack for each year
yrs<-seq(2011,2015,by=1) # Years in analysis
snow<-c(0.61325,1,0.77701,0.53818,0.415553) # Relative snowpacks for 2011-2015
snowyr<-data.frame(yrs,snow) # Create data frame
colnames(snowyr)<-c("Year","snow") # Name columns


setwd(all.input.wd) # Set Working Directory to read in files
all.fils<-list.files(getwd()) # List names of annual files

test.char<-nchar(all.fils[1])-4 # Which character in file names would indicate lag 1 data set?

#setwd(same.input.wd)

#all.fils<-list.files(getwd())
#test.char<-nchar(all.fils[1])-16

lag.fils<-all.fils[substr(all.fils,test.char,test.char)==1]

all.yrs<-read.csv(lag.fils[1],header=T)
all.yrs$Year<-as.numeric(substr(lag.fils[1],1,4))-2010
all.yrs$RelSnow<-snowyr$snow[snowyr$Year==as.numeric(substr(lag.fils[1],1,4))]

for(i in lag.fils[-1])
{
  subdat<-read.csv(i,header=T)
  subdat$Year<-as.numeric(substr(i,1,4))-2010
  subdat$RelSnow<-snowyr$snow[snowyr$Year==as.numeric(substr(i,1,4))]
  all.yrs<-rbind(all.yrs,subdat)
}

write.csv(all.yrs,file="WoodRiverTempSensSnow_1Lag.csv",row.names=F)
