# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(ggplot2);library(data.table);library(gridExtra);library(forecast);library(dplyr);library(magrittr);library(lmtest)

## WESE
# Because it is a linear regression model, we can do proper partial dependence plots
rm(list=ls())
gc()
pathToGit<-"C:/Users/lsalas/git/anthropocene/"

load(paste0(pathToGit,"data/pdPlots_WESE.RData"))

pfish<-pdat_fish %>% ggplot(aes(x=Fish_cum3,y=fitv)) +
		geom_ribbon(aes(ymin=lsd,ymax=usd),alpha=0.7,fill="gray30") + geom_line(color="black",linewidth=1) +
		theme_bw() + labs(x="Tonnes of fish >= 134 cm TL cum. 3 yrs.", y = "Predicted log(growth rate)") +
		annotate("text", label="A", x = 10, y = 0.27, color="grey20", size=5)
		
pgysp<-pdat_gysp %>% ggplot(aes(x=GyreSPD,y=fitv)) +
		geom_ribbon(aes(ymin=lsd,ymax=usd),alpha=0.7,fill="gray30") + geom_line(color="black",linewidth=1) +
		theme_bw() + labs(x="Gyre speed (km/d)", y = "") +
		annotate("text", label="B", x = 4.9, y = 0.2, color="grey20", size=5)

p_ow<-pdat_ow %>% ggplot(aes(x=mnOW_MCM6,y=fitv)) +
		geom_ribbon(aes(ymin=lsd,ymax=usd),alpha=0.7,fill="gray30") + geom_line(color="black",linewidth=1) +
		theme_bw() + labs(x="Mean open water area (km2) lag 6 yrs.", y = "Predicted log(growth rate)") +
		annotate("text", label="C", x = 1.15, y = 0.30, color="grey20", size=5)

pfie<-pdat_fie %>% ggplot(aes(x=FIE,y=fitv)) +
		geom_ribbon(aes(ymin=lsd,ymax=usd),alpha=0.7,fill="gray30") + geom_line(color="black",linewidth=1) +
		theme_bw() + labs(x="Fast Ice Extent (km)", y = "") +
		annotate("text", label="D", x = 14, y = 0.51, color="grey20", size=5)

wesepred$Year<-c(2003:2017)
ptrend<-wesepred %>% ggplot(aes(x=Year,y=pred)) + geom_point(size=1.5,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.5) + 
		geom_point(aes(x=Year,y=growthWESE),size=2,color="#ff5733") + theme_bw() +
		labs(x="Year", y = "Log(growth rate)") +
		annotate("text", label="E", x = 2003, y = 0.48, color="grey20", size=5)
pblank<-ggplot() + theme_bw() + theme(panel.border = element_blank(),panel.background = element_blank())

pwese<-grid.arrange(pfish, pgysp, p_ow, pfie, ptrend, pblank, 
		ncol = 2, nrow = 3)
dev.new();plot(pwese)

###############
## For the penguin ARIMA models we use the model and original data to plot the trends and covariate influences
## Ideally we want to regress the fitted values against each covariate
## However, the predictions have the effect of each other covariate
## To remove these we regress the covariate of interest againts all others, and retrieve the residuals
## We then add to the residuals the mean value of the covariate of interest
## This way we obtain the effect of the covariate of interest alone, and can attach the estimated SE to it and plot against the predicted growth
rm(list=ls())
gc()
pathToGit<-"C:/Users/lsalas/git/anthropocene/"
load(paste0(pathToGit,"data/Bird_topModel.RData"))

# Get predicted values and SEs
birddf<-master_gr[,c("av_SIE", "GyreSPD", "Fish_cum3", "OW_day_RSPlag4","year","growth_bird")]
birddf$pred<-fitted(mdlTop)
birddf<-subset(birddf,!is.na(pred))
birddf$sepred<-as.numeric(sapply(1:nrow(birddf),function(rr,df,mdl){
					G<-as.numeric(df[rr,1:(ncol(df)-3)])
					G<-c(0,G)
					seval<-sqrt(t(G) %*% vcov(mdl) %*% G)
					return(seval)
				},df=birddf,mdl=mdlTop))
birddf$lcl<-birddf$pred-birddf$sepred
birddf$ucl<-birddf$pred+birddf$sepred

# Get the residuals of regressing the parameter of interest vs all other covariates
siemdl<-lm(av_SIE~GyreSPD+Fish_cum3+OW_day_RSPlag4,birddf)
birddf$av_SIEcorr<-mean(birddf$av_SIE)+siemdl$residuals
#get the intercept - the slope comes from the ARIMA model
intsie<-mean(birddf$pred-(birddf$av_SIEcorr * -0.328))
psie<-birddf %>% ggplot(aes(x=av_SIEcorr,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.03) + 
		geom_abline(intercept=intsie,slope= -0.328, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="Avg. Sea Ice Extent (x 10^6 km2)", y = "Predicted Log(growth rate)") +
		annotate("text", label="A", x = 2.45, y = 0.48, color="grey20", size=5)

gyspmdl<-lm(GyreSPD~av_SIE+Fish_cum3+OW_day_RSPlag4,birddf)
birddf$GyreSPDcorr<-mean(birddf$GyreSPD)+gyspmdl$residuals
intgysp<-mean(birddf$pred-(birddf$GyreSPDcorr * 0.106))
pgysp<-birddf %>% ggplot(aes(x=GyreSPDcorr,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.1) + 
		geom_abline(intercept=intgysp,slope= 0.106, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="Gyre speed (km/d)", y = "") +
		annotate("text", label="B", x = 4.9, y = 0.48, color="grey20", size=5)

fishmdl<-lm(Fish_cum3~av_SIE+GyreSPD+OW_day_RSPlag4,birddf)
birddf$Fish_cum3corr<-mean(birddf$Fish_cum3)+fishmdl$residuals
intfish<-mean(birddf$pred-(birddf$Fish_cum3corr * 0.019))
pfish<-birddf %>% ggplot(aes(x=Fish_cum3corr,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=1) + 
		geom_abline(intercept=intfish,slope= 0.019, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="Tonnes of fish >= 134 cm TL cum. 3 yrs.", y = "Predicted Log(growth rate)") +
		annotate("text", label="C", x = 10, y = 0.48, color="grey20", size=5)

owmdl<-lm(OW_day_RSPlag4~av_SIE+Fish_cum3+GyreSPD,birddf)
birddf$OW_day_RSPlag4corr<-mean(birddf$OW_day_RSPlag4)+owmdl$residuals
intow<-mean(birddf$pred-(birddf$OW_day_RSPlag4corr * -0.030))
p_ow<-birddf %>% ggplot(aes(x=OW_day_RSPlag4corr,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.8) + 
		geom_abline(intercept=intow,slope= -0.030, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="1st Day of Open Water lag 4 yrs.", y = "") +
		annotate("text", label="D", x = 1, y = 0.48, color="grey20", size=5)

pblank<-ggplot() + theme_bw() + theme(panel.border = element_blank(),panel.background = element_blank())

ptrend<-birddf %>% ggplot(aes(x=year,y=pred)) + geom_point(size=1.5,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.5) + 
		geom_point(aes(x=year,y=growth_bird),size=2,color="#ff5733") + theme_bw() +
		labs(x="Year", y = "Log(growth rate)") +
		annotate("text", label="E", x = 2003, y = 0.48, color="grey20", size=5)

pbird<-grid.arrange(psie, pgysp, pfish, p_ow, ptrend, pblank,
		ncol = 2, nrow = 3)
dev.new();plot(pbird)

######
## Crozier
rm(list=ls())
gc()
pathToGit<-"C:/Users/lsalas/git/anthropocene/"
load(paste0(pathToGit,"data/Croz_topModel.RData"))

#we get the fitted values
crozdf<-master_gr[,c("GyreSPD_lag4", "SIE_lag4", "OW_day_RSP", "Fish_cum3","mnAirTemp_lag5","year","growth_croz")]
crozdf$pred<-fitted(topModel)
crozdf<-subset(crozdf,!is.na(pred))
crozdf$sepred<-as.numeric(sapply(1:nrow(crozdf),function(rr,df,mdl){
					G<-as.numeric(df[rr,1:(ncol(df)-3)])
					G<-c(0,G)
					seval<-sqrt(t(G) %*% vcov(mdl) %*% G)
					return(seval)
				},df=crozdf,mdl=topModel))
crozdf$lcl<-crozdf$pred-crozdf$sepred
crozdf$ucl<-crozdf$pred+crozdf$sepred

gyspmdl<-lm(GyreSPD_lag4~SIE_lag4+OW_day_RSP+Fish_cum3+mnAirTemp_lag5,crozdf)
crozdf$GyreSPD_lag4corr<-mean(crozdf$GyreSPD_lag4)+gyspmdl$residuals
intgysp<-mean(crozdf$pred-(crozdf$GyreSPD_lag4corr * 0.116))
pgysp<-crozdf %>% ggplot(aes(x=GyreSPD_lag4corr,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.1) + 
		geom_abline(intercept=intgysp,slope= 0.116, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="Gyre speed (km/d) lag 4 yrs.", y = "Predicted Log(growth rate)") +
		annotate("text", label="A", x = 4.8, y = 0.70, color="grey20", size=5)

siemdl<-lm(SIE_lag4 ~ GyreSPD_lag4 + OW_day_RSP + Fish_cum3 + mnAirTemp_lag5,crozdf)
crozdf$SIE_lag4corr<-mean(crozdf$SIE_lag4)+siemdl$residuals
intsie<-mean(crozdf$pred-(crozdf$SIE_lag4corr * -0.261))
psie<-crozdf %>% ggplot(aes(x=SIE_lag4corr,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.02) + 
		geom_abline(intercept=intsie,slope= -0.261, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="Avg. Sea Ice Extent (x 10^6 km2) lag 4 yrs.", y = "") +
		annotate("text", label="B", x = 2.45, y = 0.70, color="grey20", size=5)

owmdl<-lm(OW_day_RSP ~ GyreSPD_lag4 + SIE_lag4 + Fish_cum3 + mnAirTemp_lag5,crozdf)
crozdf$OW_day_RSPcorr<-mean(crozdf$OW_day_RSP)+owmdl$residuals
intow<-mean(crozdf$pred-(crozdf$OW_day_RSPcorr * -0.018))
p_ow<-crozdf %>% ggplot(aes(x=OW_day_RSP,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=1) + 
		geom_abline(intercept=intow,slope= -0.018, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="1st Day of Open Water", y = "Predicted Log(growth rate)") +
		annotate("text", label="C", x = 0, y = 0.70, color="grey20", size=5)

fishmdl<-lm(Fish_cum3 ~ GyreSPD_lag4 + OW_day_RSP + SIE_lag4 + mnAirTemp_lag5,crozdf)
crozdf$Fish_cum3corr<-mean(crozdf$Fish_cum3)+fishmdl$residuals
intfish<-mean(crozdf$pred-(crozdf$Fish_cum3corr * 0.005))
pfish<-crozdf %>% ggplot(aes(x=Fish_cum3corr,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=1) + 
		geom_abline(intercept=intfish,slope= 0.005, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="Tonnes of fish >= 134 cm TL cum. 3 yrs.", y = "") +
		annotate("text", label="D", x = 10, y = 0.70, color="grey20", size=5)

airmdl<-lm(mnAirTemp_lag5 ~ GyreSPD_lag4 + SIE_lag4 + Fish_cum3 + OW_day_RSP,crozdf)
crozdf$mnAirTemp_lag5corr<-mean(crozdf$mnAirTemp_lag5)+airmdl$residuals
intair<-mean(crozdf$pred-(crozdf$mnAirTemp_lag5corr * -0.063))
p_air<-crozdf %>% ggplot(aes(x=mnAirTemp_lag5corr,y=pred)) + geom_point(size=2,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.12) + 
		geom_abline(intercept=intair,slope= -0.063, color="blue",linewidth=1.2) +
		theme_bw() + labs(x="Mean Air Temperature (°C) lag 5 yrs.", y = "Predicted Log(growth rate)") +
		annotate("text", label="E", x = -7.5, y = 0.70, color="grey20", size=5)

ptrend<-crozdf %>% ggplot(aes(x=year,y=pred)) + geom_point(size=1.5,color="black") + 
		geom_errorbar(aes(ymin=lcl,ymax=ucl),linewidth=1,color="black", width=0.5) + 
		geom_point(aes(x=year,y=growth_croz),size=2,color="#ff5733") + theme_bw() +
		labs(x="Year", y = "Log(growth rate)") +
		annotate("text", label="F", x = 2003, y = 0.70, color="grey20", size=5)

pcroz<-grid.arrange(pgysp, psie, p_ow, pfish, p_air, ptrend, 
		ncol = 2, nrow = 3)
dev.new();plot(pcroz)


