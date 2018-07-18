# format data for unmarked and fit models -- UNBURNED POINTS

# Paul J. Taillie
# North Carolina State University
# 1/1/18


# #clear environment
remove(list=ls())

# load packages
library(tidyverse)
library(unmarked)
library(AICcmodavg)

# -------  load data  --------------------------------
covs<-<-read.csv("processed_data/siteCovs_formatted.csv")
covs<-covs[covs$Burned<1,] #remove burned points

birds<-read.csv("raw_data/bird_data.csv")
birds<-birds[birds$Distance<=150,] # remove detections farther than 150m
birds$PointYrID<-as.character(birds$PointYrID)
breaks=c(0,5,10,15,20,25,30,35,40,45,50,60,70,80,90,100,120,150)  # bin data

# List of species to analyze, with best key function from preliminary analysis
species.all<-read.csv("processed_data/species.csv",header=T)   
species.all$SPEC<-as.character(species.all$SPEC)
species.all$model<-as.character(species.all$model)

#revise key function
for (i in 1:nrow(species.all)){
  if (species.all$model[i]=="mod1"){species.all$model[i]<-"halfnorm"}
  if (species.all$model[i]=="mod2"){species.all$model[i]<-"hazard"}
  if (species.all$model[i]=="mod3"){species.all$model[i]<-"uniform"}
}

# extract bird data from relevant points and years

index1<-which(covs$PointYrID%in%birds$PointYrID==F)
covs<-covs[-index1,]
index.bird.pts<-which(birds$PointYrID%in%covs$PointYrID==T)
birds<-birds[index.bird.pts,]


# standardize covariates   -----------------------
temp<-rep(NA,nrow(covs))
covs.temp=data.frame(temp,temp,temp)




slope.mean = mean(covs$slope,na.rm=T)
slope.sd = sd(covs$slope,na.rm=T)
covs.temp$slope= (covs$slope-slope.mean)/slope.sd

aspect.mean = mean(covs$aspect,na.rm=T)
aspect.sd = sd(covs$aspect,na.rm=T)
covs.temp$aspect= (covs$aspect-aspect.mean)/aspect.sd

elevation.mean = mean(covs$elevation,na.rm=T)
elevation.sd = sd(covs$elevation,na.rm=T)
covs.temp$elevation= (covs$elevation-elevation.mean)/elevation.sd

tree.mean = mean(covs$TreeCov,na.rm=T)
tree.sd = sd(covs$TreeCov,na.rm=T)
covs.temp$tree= (covs$TreeCov-tree.mean)/tree.sd




covs.temp$prefire=as.factor(covs$prefire)
covs.temp$prefire <- relevel(covs.temp$prefire, ref="late closed")

#covs.temp$burned<-factor(covs$Burned)
covs.temp$visits<-covs$visits
cov.table<-covs.temp[,4:9]

# fix PSFL -> WEFL
birds$Spp<-as.character(birds$Spp)
index.1=which(birds$Spp=="PSFL")
birds$Spp[index.1]<-"WEFL"

birds$PointYrID<-factor(birds$PointYrID)

# prepare stuff for predicted densities
unburned.preds<-data.frame(
  mean=rep(NA,nrow(species.all)),
  lower=rep(NA,nrow(species.all)),
  upper=rep(NA,nrow(species.all)))
new.data=data.frame(
  prefire=levels(covs.temp$prefire),
  slope=0,
  aspect=0,
  elevation=0,
  visits=1)

#   Start of model fitting loop   -----------------------------------
for (i in 1:nrow(species.all)){
  #set up unmarked frame
  spec.dat<-birds[birds$Spp==species.all$SPEC[i],]
  ydat=formatDistData(spec.dat,
                      distCol="Distance",
                      transectNameCol="PointYrID",
                      dist.breaks=breaks)
  
  umf=unmarkedFrameDS(y=ydat, siteCovs=cov.table,
                      dist.breaks=breaks,
                      unitsIn="m",survey="point") 
  
  mod1<- distsamp(~tree ~ prefire-1+
                    elevation+slope+aspect+ I(elevation^2)+
                    offset(log(visits)), data=umf, output="density", keyfun=species.all$model[i],rel.tol=0.01)
  
  
  preds.temp=predict(mod1,type="state",newdata=new.data)
  
  max.index<-which.max(preds.temp$Predicted)
  unburned.preds[i,]<-c(preds.temp$Predicted[max.index],
                        preds.temp$lower[max.index], 
                        preds.temp$upper[max.index])
  
}


write.csv(unburned.preds,file="processed_data/results_unburned.csv")











