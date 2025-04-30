# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(ggplot2);library(data.table);library(gridExtra);library(forecast);library(dplyr);library(magrittr);library(lmtest)
library(fmsb); library(stats); library(modelr); library(nlme)
pathToGit<-"C:/Users/lsalas/git/anthropocene/"
load(paste0(pathToGit,"data/Croz_topModel.RData"))

densdf<-read.csv("C:/Users/lsalas/Desktop/croz_meanPropChange_v2023-03-22.csv", stringsAsFactors=F)

densdf<-densdf[,c("Season","PropChange_active","SE_active")]
names(densdf)<-gsub("Season","Year",names(densdf))
densdf$abs_change<-densdf$PropChange_active+1
densdf$growth_Dens<-log(densdf$abs_change)
densdf$udiff<-densdf$abs_change + (1.96*densdf$SE_active)
densdf$ldiff<-densdf$abs_change - (1.96*densdf$SE_active)
densdf$ucl_Growth<-log(densdf$udiff)
densdf$lcl_Growth<-log(densdf$ldiff)
countdf<-master_gr[,c("year","growth_croz","GyreSPD_lag4", "SIE_lag4", "OW_day_RSP", "Fish_cum3","mnAirTemp_lag5")]
names(countdf)<-c("Year","growth_Count","GyreSPD_lag4", "SIE_lag4", "OW_day_RSP", "Fish_cum3","mnAirTemp_lag5")
testdf<-merge(countdf,densdf[,c("Year","growth_Dens","ucl_Growth","lcl_Growth")],by="Year")

ggplot(testdf,aes(x=Year)) + geom_pointrange(aes(y=growth_Dens,ymin=lcl_Growth,ymax=ucl_Growth),color="black") + 
		geom_point(aes(y=growth_Count), color="red") + theme_bw() + scale_x_continuous(breaks=seq(2003,2019,by=2))

### Trying the top model on these data
cov_sat_growth_top<-as.matrix(testdf[,c("GyreSPD_lag4", "SIE_lag4", "OW_day_RSP", "Fish_cum3","mnAirTemp_lag5")]) 
#"GyreSPD", "SIE_lag4", "OW_day_RSPlag4", "Fish_cum3","mnAirTemp_lag5"
countModel<-Arima(testdf$growth_Count,order=c(1,0,0),include.mean=F,include.drift=F,xreg= cov_sat_growth_top)
summary(countModel)
coeftest(countModel)  #This is our result

densModel<-Arima(testdf$growth_Dens,order=c(1,0,0),include.mean=F,include.drift=F,xreg= cov_sat_growth_top)
summary(densModel)
coeftest(densModel)
