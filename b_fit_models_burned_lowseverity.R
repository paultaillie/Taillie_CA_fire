# format data for unmarked and fit models  --  BURNED POINTS

## This is identical to "b_fit_models_burned.R" but calculates AVERAGE densities
#     for figure 2b

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
covs<-covs[covs$Burned>0,] #remove unburned points

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

#index1<-which(covs$PointYrID%in%birds$PointYrID==F)
#covs<-covs[-index1,]
index.bird.pts<-which(birds$PointYrID%in%covs$PointYrID==T)
birds<-birds[index.bird.pts,]


# standardize covariates   -----------------------
temp<-rep(NA,nrow(covs))
covs.temp=data.frame(temp,temp,temp)


burnsev.mean = mean(covs$burnsev,na.rm=T)
burnsev.sd = sd(covs$burnsev,na.rm=T)
covs.temp$burnsev <- (covs$burnsev-burnsev.mean)/burnsev.sd

tsb.mean = mean(covs$TSB,na.rm=T)
tsb.sd = sd(covs$TSB,na.rm=T)
covs.temp$tsb = (covs$TSB-tsb.mean)/tsb.sd

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

shrub.mean = mean(covs$ShrubCov,na.rm=T)
shrub.sd = sd(covs$ShrubCov,na.rm=T)
covs.temp$shrub= (covs$ShrubCov-shrub.mean)/shrub.sd

snag.mean = mean(covs$SnagBA,na.rm=T)
snag.sd = sd(covs$SnagBA,na.rm=T)
covs.temp$snag= (covs$SnagBA-snag.mean)/snag.sd

covs.temp$prefire=as.factor(covs$prefire)
covs.temp$prefire <- relevel(covs.temp$prefire, ref="late closed")

#covs.temp$burned<-factor(covs$Burned)
covs.temp$visits<-covs$visits
cov.table<-covs.temp[,4:13]

# fix PSFL -> WEFL
birds$Spp<-as.character(birds$Spp)
index.1=which(birds$Spp=="PSFL")
birds$Spp[index.1]<-"WEFL"

birds$PointYrID<-factor(birds$PointYrID)

results.out<-as.data.frame(matrix(NA,50,47))


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
  
  # fit models to compare
  mod1<- distsamp(~snag+tree ~ prefire-1+
                    elevation+slope+aspect+ I(elevation^2)+
                    tsb+
                    burnsev+
                    I(burnsev^2)+
                    I(tsb^2)+
                    burnsev:tsb+
                    offset(log(visits)), data=umf, output="density", keyfun=species.all$model[i],rel.tol=0.01)
  
  mod1.means<-coef(mod1,type="state") #collect means
  mod1.ci<-confint(mod1,type="state") #collect CI's
  
  # predict peak density
  tsb.range = seq(1,15)
  burnsev.range = seq(0,100,,15)
  x <- (tsb.range - tsb.mean)/tsb.sd
  y <- (burnsev.range - burnsev.mean)/burnsev.sd
  w <- rep(0,15)
  
  z1=data.frame(
    slope=w,
    aspect=w,
    elevation=w,
    prefire=levels(covs$prefire)[1],
    burnsev=rep(y[1],15),
    tsb=x,
    visits=1)
  
  for (j in 2:15){
    z.temp1=data.frame(
      slope=w,
      aspect=w,
      elevation=w,
      prefire=levels(covs$prefire)[1],
      burnsev=rep(y[j],15),
      tsb=x,
      visits=1)
    
    z1=rbind(z1,z.temp1)
  }
  
  z2=data.frame(
    slope=w,
    aspect=w,
    elevation=w,
    prefire=levels(covs$prefire),
    burnsev=y[1],
    tsb=x[1],
    visits=1)
  z=rbind(z1,z2)
  
  peak.preds<-predict(mod1,type="state",newdata=z, appendData=T)
  
  index.max<-which.max(peak.preds$Predicted)
  index.av<-which(peak.preds$burnsev==peak.preds$burnsev[index.max])
  pred.av<-mean(peak.preds$Predicted[index.av])
  pred.av.lower<-mean(peak.preds$lower[index.av])
  pred.av.upper<-mean(peak.preds$upper[index.av])
  
  
  
  results.out[i,]<-c(mod1.means,
                     mod1.ci[,1],
                     mod1.ci[,2],
                     pred.av,
                     pred.av.lower,
                     pred.av.upper,
                     peak.preds$burnsev[index.max],
                     peak.preds$tsb[index.max])
  
} #end model fitting loop loop


colnames(results.out)<-c(names(mod1@estimates@estimates$state@estimates),
                         paste("lower",names(mod1@estimates@estimates$state@estimates)),
                         paste("upper",names(mod1@estimates@estimates$state@estimates)),
                         "av.density",
                         "av.density.lower",
                         "av.density.upper",
                         "peak.burnsev",
                         "peak.tsb")

rownames(results.out)<-species.all$SPEC

write.csv(results.out,file="processed_data/results_burned_lowsev.csv")





