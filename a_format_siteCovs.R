# Preliminary formatting of site covariates and calulation of prefire condition

# Paul J. Taillie
# North Carolina State University
# 1/1/18

# load packages
library(tidyverse)

# #clear environment
remove(list=ls())

# load packages
library(tidyverse)

#load data
dat<-read.csv("raw_data/ArcGIS_covs.csv")

#burn severity
table(dat$Fire_ID)
dat$Fire_ID<-as.character(dat$Fire_ID)
for (i in 1:nrow(dat)){
  if (dat$Fire_ID[i]=="2000Storrie") {dat$CC_BurnSev[i]<-dat$cc_storrie[i]}
  if (dat$Fire_ID[i]=="2000STORRIE") {dat$CC_BurnSev[i]<-dat$cc_storrie[i]}
  if (dat$Fire_ID[i]=="2001Star")    {dat$CC_BurnSev[i]<-dat$cc_star[i]}
  if (dat$Fire_ID[i]=="2004Straylor") {dat$CC_BurnSev[i]<-dat$cc_straylo[i]}
  if (dat$Fire_ID[i]=="2006Boulder") {dat$CC_BurnSev[i]<-dat$cc_boulder[i]}
  if (dat$Fire_ID[i]=="2007Moonlight") {dat$CC_BurnSev[i]<-dat$cc_moonlit[i]}
  if (dat$Fire_ID[i]=="2008Cold")      {dat$CC_BurnSev[i]<-dat$cc_cold[i]}
  if (dat$Fire_ID[i]=="2008Cub") {dat$CC_BurnSev[i]<-dat$cc_cub[i]}
  if (dat$Fire_ID[i]=="2008Rich") {dat$CC_BurnSev[i]<-dat$cc_rich[i]}
  if (dat$Fire_ID[i]=="2012Chips") {dat$CC_BurnSev[i]<-dat$cc_chips[i]}
  if (dat$Fire_ID[i]=="2014Bald") {dat$CC_BurnSev[i]<-dat$cc_bald[i]}
  if (dat$Fire_ID[i]=="unburned") {dat$CC_BurnSev[i]<-0}}





#extract WHR data based on burned vs. unburned
dat$WHRtype=rep(NA,nrow(dat))
dat$WHRsize=rep(NA,nrow(dat))
dat$WHRdens=rep(NA,nrow(dat))


for (i in 1:nrow(dat)){
  if(dat$Burned[i]==1) {   #burned points get older WHR data
    dat$WHRtype[i]<-as.character(dat$WHRtype2[i])
    dat$WHRsize[i]<-as.numeric(dat$WHRsize2[i])
    dat$WHRdens[i]<-as.character(dat$WHRdens2[i])
  }
  if(dat$Burned[i]==0) {
    dat$WHRtype[i]<-as.character(dat$WHRtype15[i])
    dat$WHRsize[i]<-as.numeric(dat$WHRsize15[i])
    dat$WHRdens[i]<-as.character(dat$WHRdens15[i])
  }
} #end loop


#convert density to same scale
for (i in 1:nrow(dat)){
  if (dat$WHRdens[i]=="X"){dat$WHRdens[i]<-0}
  if (dat$WHRdens[i]=="S"){dat$WHRdens[i]<-1}
  if (dat$WHRdens[i]=="P"){dat$WHRdens[i]<-2}
  if (dat$WHRdens[i]=="M"){dat$WHRdens[i]<-3}
  if (dat$WHRdens[i]=="D"){dat$WHRdens[i]<-4}
}

dat$WHRdens[which(dat$WHRdens==9999)]<--9999
dat$WHRsize[which(dat$WHRdens==9999)]<--9999


# WHRtype revise categories
for (i in 1:nrow(dat)){
  
  if(dat$WHRtype[i]=="AGS"){dat$WHRtype[i]<-"remove"}
  if(dat$WHRtype[i]=="BAR"){dat$WHRtype[i]<- "shrub"}
  if(dat$WHRtype[i]=="DFR"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="EPN"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="JPN"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="JUN"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="LPN"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="MCH"){dat$WHRtype[i]<- "shrub"}
  if(dat$WHRtype[i]=="MCP"){dat$WHRtype[i]<- "shrub"}
  if(dat$WHRtype[i]=="MHC"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="MHW"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="MRI"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="PPN"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="RFR"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="SGB"){dat$WHRtype[i]<- "shrub"}
  if(dat$WHRtype[i]=="SMC"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="WAT"){dat$WHRtype[i]<-"remove"}
  if(dat$WHRtype[i]=="WFR"){dat$WHRtype[i]<-"forest"}
  if(dat$WHRtype[i]=="PGS"){dat$WHRtype[i]<-"remove"}
}

dat2<-dat[which(dat$WHRtype!="remove"),]

dat2$prefire<-as.character(dat2$prefire)
# Incorporate size and density
for (i in 1:nrow(dat2)){
  if (dat2$WHRtype[i]=="shrub") {dat2$prefire[i]<-"Shrub"}
  if (dat2$WHRtype[i]=="forest") {
    
    if (dat2$WHRsize[i]==0){dat2$prefire[i]<-"Shrub"}
    if (dat2$WHRsize[i]==1){dat2$prefire[i]<-"Shrub"}
    if (dat2$WHRsize[i]==2){dat2$prefire[i]<-"Shrub"}
    if (dat2$WHRsize[i]==3){dat2$prefire[i]<-"young.forest"}
    if (dat2$WHRsize[i]>3&dat2$WHRdens[i]<3){dat2$prefire[i]<-"open"}
    if (dat2$WHRsize[i]==4&dat2$WHRdens[i]>=3 ){dat2$prefire[i]<-"mid closed"}
    if (dat2$WHRsize[i]==5&dat2$WHRdens[i]>=3 ){dat2$prefire[i]<-"late closed"}
  }
  
} #end loop

#check blanks and look at table
temp.blank<-which(dat2$prefire==" ")
table(dat2$prefire)


# Re-calculate TSB
dat2$TSB[dat2$Burned==0]<-0
dat2$TSB=dat2$Year-dat2$Fire_year
index.check=which(dat2$Fire_ID=="2012Chips")

dat2[index.check,6:9]
dat2$TSB[dat2$Burned==0]<-0


#check elevation
covs<-dat2

# pull out variables of interest (elevation, habitat type)
dat4<-data.frame(
  point=covs$PointID,
  pointyr=covs$PointYrID,
  burned=as.factor(covs$Burned),
  elev=covs$elevation)

# elevation plot
elev.plot= ggplot(dat4, aes(x=as.numeric(elev),y=..count../sum(..count..)))+
  geom_histogram(binwidth=100,aes(fill=burned),position="dodge")
elev.plot

#separate into burned and unburned
dat.burn=dat4[dat4$burned==1,]
dat.unburn=dat4[dat4$burned==0,]
#dat.unburn=dat.unburn[dat.unburn$elev<2175,]

elev.plot.burn= ggplot(dat.burn, aes(x=elev,y=..count../sum(..count..)))+
  geom_histogram(binwidth=100)+
  xlim(800,2600)+
  geom_vline(xintercept = mean(dat.burn$elev), size = 1, colour = "#FF3721",
             linetype = "dashed")+
  ggtitle("Burned")+
  ylab("Proportional Frequency")+
  xlab("Elevation (m)")

elev.plot.unburn= ggplot(dat.unburn, aes(x=elev,y=..count../sum(..count..)))+
  geom_histogram(binwidth=100)+
  xlim(800,2600)+
  geom_vline(xintercept = mean(dat.unburn$elev), size = 1, colour = "#FF3721",
             linetype = "dashed")+
  ggtitle("Unburned")+
  ylab("Proportional Frequency")+
  xlab("Elevation (m)")

grid.arrange(elev.plot.burn,elev.plot.unburn,nrow=2)





#remove high elevation points
high.elev<-max(dat.burn$elev)
dat2<-dat2[dat2$elevation<high.elev,]


# write to csv
dat.out<-data.frame(
  PointYrID=dat2$PointYrID,
  PointID=dat2$PointID,
  FIRE_ID=dat2$Fire_ID,
  Fire_year=dat2$Fire_year,
  Year=dat2$Year,
  TSB=dat2$TSB,
  TreeCov=dat2$TreeCov,
  ShrubCov=dat2$ShrubCov,
  SnagBA=dat2$SnagBA,
  lat=dat2$lat,
  long=dat2$long,
  burnsev=dat2$CC_BurnSev,
  slope=dat2$slope,
  elevation=dat2$elevation,
  aspect=dat2$aspect,
  visits=dat2$visits,
  prefire=dat2$prefire,
  Burned=dat2$Burned)

write.csv(dat.out,file="processed_data/siteCovs_formatted.csv")

