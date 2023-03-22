# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(ggplot2);library(data.table)

###############
rm(list=ls())
gc()
## First Bird ADPE
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Anthropocene/pdPlots_bird.RData")

names(pdat_fish)<-gsub("Fish_cum3","Value",names(pdat_fish))
pdat_fish$Covariate<-"Fish Biomass cum. 3 yrs."

names(pdat_gysp)<-gsub("GyreSPD","Value",names(pdat_gysp))
pdat_gysp$Covariate<-"Gyre Speed"

names(pdat_ow)<-gsub("OW_day_RSPlag4","Value",names(pdat_ow))
pdat_ow$Covariate<-"Open Water 1st Day RS Polynya lag 4 yrs."

names(pdat_sie)<-gsub("av_SIE","Value",names(pdat_sie))
pdat_sie$Covariate<-"Avg. Sea Ice Extent"

pdat_bird_list<-list(pdat_fish,pdat_gysp,pdat_ow,pdat_sie)
pdat_bird<-rbindlist(pdat_bird_list)

pbird<-ggplot(pdat_bird,aes(x=Value,y=meanv)) + geom_ribbon(aes(ymin=lcv,ymax=ucv),alpha=0.7,fill="gray30") + geom_line(color="black",size=1) +
		facet_wrap(~Covariate,scales="free_x",ncol=2) + theme_bw() +
		labs(x="Covariate value", y = "Predicted log(growth rate)")
dev.new();print(pbird)

#################
rm(list=ls())
gc()
## Crozier ADPE
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Anthropocene/pdPlots_Croz.RData")

names(pdat_fish)<-gsub("Fish_cum3","Value",names(pdat_fish))
pdat_fish$Covariate<-"Fish Biomass cum. 3 yrs."

names(pdat_gysp)<-gsub("GyreSPD_lag4","Value",names(pdat_gysp))
pdat_gysp$Covariate<-"Gyre Speed lag 4 yrs."

names(pdat_ow)<-gsub("OW_day_RSP","Value",names(pdat_ow))
pdat_ow$Covariate<-"Open Water 1st Day RS Polynya"

names(pdat_sie)<-gsub("SIE_lag4","Value",names(pdat_sie))
pdat_sie$Covariate<-"Avg. Sea Ice Extent lag 4 yrs."

names(pdat_airt)<-gsub("mnAirTemp_lag5","Value",names(pdat_airt))
pdat_airt$Covariate<-"Mean Air Temp. lag 5 yrs."

pdat_croz_list<-list(pdat_fish,pdat_gysp,pdat_ow,pdat_sie,pdat_airt)
pdat_croz<-rbindlist(pdat_croz_list)

pcroz<-ggplot(pdat_croz,aes(x=Value,y=meanv)) + geom_ribbon(aes(ymin=lcv,ymax=ucv),alpha=0.7,fill="gray30") + geom_line(color="black",size=1) +
		facet_wrap(~Covariate,scales="free_x",ncol=2) + theme_bw() +
		labs(x="Covariate value", y = "Predicted log(growth rate)")

dev.new();print(pcroz)

####################
rm(list=ls())
gc()
## WESE
load("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Anthropocene/pdPlots_WESE.RData")

names(pdat_fish)<-gsub("Fish_cum3","Value",names(pdat_fish))
pdat_fish$Covariate<-"Fish Bbiomass cum. 3 yrs."

names(pdat_gysp)<-gsub("GyreSPD","Value",names(pdat_gysp))
pdat_gysp$Covariate<-"Gyre Speed"

names(pdat_ow)<-gsub("mnOW_MCM6","Value",names(pdat_ow))
pdat_ow$Covariate<-"Mean Open Water Day McM Sound lag 6 yrs."

names(pdat_fie)<-gsub("FIE","Value",names(pdat_fie))
pdat_fie$Covariate<-"Fast Ice Extent"

pdat_wese_list<-list(pdat_fish,pdat_gysp,pdat_ow,pdat_fie)
pdat_wese<-rbindlist(pdat_wese_list)

pwese<-ggplot(pdat_wese,aes(x=Value,y=fitv)) + geom_ribbon(aes(ymin=lsd,ymax=usd),alpha=0.7,fill="gray30") + geom_line(color="black",size=1) +
		facet_wrap(~Covariate,scales="free_x",ncol=2) + theme_bw() +
		labs(x="Covariate value", y = "Predicted log(growth rate)")

dev.new();print(pwese)



