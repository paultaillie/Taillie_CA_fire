# Summarize results and produce figures

# Paul J. Taillie
# North Carolina State University
# 1/4/18

# #clear environment
remove(list=ls())

# load packages
library(ggplot2)
library(ggrepel)
library(lattice)
library(gridExtra)
library(reshape2)
library(unmarked)
library(reshape2)
library(tidyverse)

#  ------ load data ----------------------------------
results.burn<-read.csv("processed_data/results_burned.csv")  
results.burn$SPEC<-as.character(results.burn$SPEC)
results.unburn<-read.csv("processed_data/results_unburned.csv")
results.burn$green.density=results.unburn$mean
results.burn$green.density.low=results.unburn$lower
results.burn$green.density.high=results.unburn$upper
covs<-read.csv(file="processed_data/siteCovs_formatted.csv")
dat<-covs[covs$Burned>0,]
low.sev.in<-read.csv(file="processed_data/results_burned_lowsev.csv")

# get rid of species with high standard errors
index.junk<-c(which(results.burn$SPEC=="WEFL"),
              which(results.burn$SPEC=="WEBL"),
              which(results.burn$SPEC=="LEGO"),
              which(results.burn$SPEC=="PIWO"),
              which(results.burn$SPEC=="ANHU"),
              which(results.burn$SPEC=="CAHU"))
results.burn<-results.burn[-index.junk,]


# -- Figure 3a  -------------------------------
#collect data
densities.burned<-data.frame(
  species=factor(results.burn$SPEC,levels=results.burn$SPEC[order(results.burn$peak.density)]),
  burned=1,
  density=results.burn$peak.density,
  lower=results.burn$peak.density.lower,
  upper=results.burn$peak.density.upper)

densities.unburned<-data.frame(
  species=factor(results.burn$SPEC,levels=results.burn$SPEC[order(results.burn$peak.density)]),
  burned=0,
  density=results.burn$green.density,
  lower=results.burn$green.density.low,
  upper=results.burn$green.density.high)
densities<-rbind(densities.burned,densities.unburned) #combine burned and unburned

# build plot
tiff("figures/figure3a.tiff",
     width=4,
     height=5,
     units="in",
     res=300,
     compression="lzw")

densities%>%
  ggplot()+
  geom_pointrange(aes(x=species,
                      y=density,
                      color=factor(burned),
                      ymin = lower, 
                      ymax = upper),position=position_dodge(width=.2))+
  scale_color_manual(name=" ",breaks=c(1,0),values=c("black","red"),labels=c("Burned","Unburned Forest"))+
  coord_flip()+
  xlab("Species")+
  ylab("Density (#/ha)")+
  theme(axis.text.x=element_text(color="black"),
        axis.text.y=element_text(color="black"))
dev.off()


# ---------   Figure 3b  ----------------------------------------------------
# This code chunk will re-write objects for Figure 3a above

# change low severity species to av density 
low.sev<-low.sev.in[,c(1,44,45,46)]
results.burn[results.burn$SPEC=="RBSA",44:46]<-low.sev[low.sev$X=="RBSA",2:4]
results.burn[results.burn$SPEC=="GCKI",44:46]<-low.sev[low.sev$X=="GCKI",2:4]
results.burn[results.burn$SPEC=="PUFI",44:46]<-low.sev[low.sev$X=="PUFI",2:4]
results.burn[results.burn$SPEC=="HETH",44:46]<-low.sev[low.sev$X=="HETH",2:4]
results.burn[results.burn$SPEC=="WHWO",44:46]<-low.sev[low.sev$X=="WHWO",2:4]
results.burn[results.burn$SPEC=="PISI",44:46]<-low.sev[low.sev$X=="PISI",2:4]
results.burn[results.burn$SPEC=="CAFI",44:46]<-low.sev[low.sev$X=="CAFI",2:4]
results.burn[results.burn$SPEC=="CAVI",44:46]<-low.sev[low.sev$X=="CAVI",2:4]
results.burn[results.burn$SPEC=="HAFL",44:46]<-low.sev[low.sev$X=="HAFL",2:4]
results.burn[results.burn$SPEC=="WAVI",44:46]<-low.sev[low.sev$X=="WAVI",2:4]
results.burn[results.burn$SPEC=="MOCH",44:46]<-low.sev[low.sev$X=="MOCH",2:4]
results.burn[results.burn$SPEC=="DEJU",44:46]<-low.sev[low.sev$X=="DEJU",2:4]
results.burn[results.burn$SPEC=="RBNU",44:46]<-low.sev[low.sev$X=="RBNU",2:4]
results.burn[results.burn$SPEC=="HEWA",44:46]<-low.sev[low.sev$X=="HEWA",2:4]
results.burn[results.burn$SPEC=="YRWA",44:46]<-low.sev[low.sev$X=="YRWA",2:4]
results.burn[results.burn$SPEC=="WETA",44:46]<-low.sev[low.sev$X=="WETA",2:4]
# -- Compare Peak Densities  -------------------------------
#collect data
densities.burned<-data.frame(
  species=factor(results.burn$SPEC,levels=results.burn$SPEC[order(results.burn$peak.density)]),
  burned=1,
  density=results.burn$peak.density,
  lower=results.burn$peak.density.lower,
  upper=results.burn$peak.density.upper)



densities.unburned<-data.frame(
  species=factor(results.burn$SPEC,levels=results.burn$SPEC[order(results.burn$peak.density)]),
  burned=0,
  density=results.burn$green.density,
  lower=results.burn$green.density.low,
  upper=results.burn$green.density.high)
densities<-rbind(densities.burned,densities.unburned) #combine burned and unburned

# build plot
tiff("figures/figure3b.tiff",
     width=4,
     height=5,
     units="in",
     res=300,
     compression="lzw")

densities%>%
  ggplot()+
  geom_pointrange(aes(x=species,
                      y=density,
                      color=factor(burned),
                      ymin = lower, 
                      ymax = upper),position=position_dodge(width=.2))+
  scale_color_manual(name=" ",breaks=c(1,0),values=c("black","red"),labels=c("Burned","Unburned Forest"))+
  coord_flip()+
  xlab("Species")+
  ylab("Density (#/ha)")+
  theme(axis.text.x=element_text(color="black"),
        axis.text.y=element_text(color="black"))
dev.off()

# -----------------------------------------------------------------------------
# list of significant species
dense.sig<-data.frame(densities.burned,densities.unburned[,3:5])
colnames(dense.sig)<-c(colnames(dense.sig)[1:5],"density.green", "lower.green", "upper.green")
dense.sig$diff=rep(0,nrow(dense.sig))
#for (i in 1:nrow(dense.sig)){
#  if(dense.sig$upper[i]<dense.sig$lower.green[i]){dense.sig$diff[i]<-(-1)}
#  if(dense.sig$upper.green[i]<dense.sig$lower[i]){dense.sig$diff[i]<-(1)}
#}

sig<-rep("No Difference",nrow(dense.sig))
sig[which(dense.sig$upper<dense.sig$lower.green)]<-"Unburned"
sig[which(dense.sig$upper.green<dense.sig$lower)]<-"Burned"
sig[which(dense.sig$species=="BBWO")]<-"Burned"
sig[which(dense.sig$species=="MOBL")]<-"Burned"
table(sig)
# Figure 2 (requires previous code chunk ----------------------------------

burnsev.raw=dat$burnsev
tsb.raw=dat$TSB

burnsev.mean = mean(burnsev.raw,na.rm=T)
burnsev.sd = sd(burnsev.raw,na.rm=T)

tsb.mean = mean(tsb.raw,na.rm=T)
tsb.sd = sd(tsb.raw,na.rm=T)

plot5.dat<-data.frame(
  burnsev=((results.burn$peak.burnsev*burnsev.sd)+burnsev.mean),
  tsb=((results.burn$peak.tsb*tsb.sd)+tsb.mean),
  sig=factor(sig),
  species=results.burn$SPEC)

#Build plot
tiff("figures/figure2.tiff",
     width=10,
     height=8,
     units="in",
     res=300,
     compression="lzw")

ggplot(plot5.dat, aes(x=tsb,y=burnsev,label=species))+
  geom_point()+
  geom_label_repel(aes(fill=sig),size=2.5)+
  scale_fill_manual(name="Maximum Density",values = c("#F8766D","grey", "#00BA38")) +
  xlab("Time Since Fire (years)")+
  ylab("Burn Severity (% Reduction in Canopy Cover)")+ 
  scale_x_discrete(limits=as.character(1:15))
dev.off()

# Figure 4  - Covariate plots  ----------------------------------


#   param.in="I.tsb.2."
coef.plot<-function(param.in,results.full){
  #parameter of interest
  index<-as.numeric(which(colnames(results.full)==param.in))
  index.lower<-index+14
  index.upper<-index+28
  indices<-c(index,index.lower,index.upper,1)
  param.dat<-results.full[,indices]
  colnames(param.dat)<-c("param","lower","upper","species")
  #sort data
  #dat.sort<-results[order(results[,index]),]
  
  #which significant
  for (i in 1:nrow(param.dat)){
    if(param.dat$lower[i]<0){
      if(param.dat$upper[i]<0){param.dat$sig[i]<-(1)}
      if(param.dat$upper[i]>0){param.dat$sig[i]<-0}
    }
    if(param.dat$upper[i]>0){
      if(param.dat$lower[i]<0){param.dat$sig[i]<-0}
      if(param.dat$lower[i]>0){param.dat$sig[i]<-1}
    }
  }
  
  #build plot
  title="broken"
  if(param.in=="I.tsb.2."){title<-expression("TSF"^2)}
  if(param.in=="tsb.burnsev"){title<-"Interaction"}
  if(param.in=="I.burnsev.2."){title<-expression("Severity"^2)}
  
  
  
  tsb1<- ggplot(param.dat, aes(x=factor(species,levels=species[order(param)]),
                               order=species[order(param)],
                               param,
                               ymin = lower, 
                               ymax = upper))
  
  tsb1+geom_pointrange(aes(size=factor(sig)))+
    scale_size_manual(guide=FALSE,values=c(.5,1.2))+
    coord_flip() +
    ggtitle(title) +
    xlab("Species") + 
    ylab(" ") +
    ylim(min(param.dat$lower),max(param.dat$upper))
} #end function

#build plot
plot1<-coef.plot(param.in="I.tsb.2.",results.full=results.burn)
plot2<-coef.plot(param.in="tsb.burnsev",results.full=results.burn)
plot3<-coef.plot(param.in="I.burnsev.2.",results.full=results.burn)
tiff("figures/figure4.tiff",
     width=7,
     height=7,
     units="in",
     res=300,
     compression="lzw")
grid.arrange(plot1,plot2,plot3,nrow=1,ncol=3)
dev.off()

#length(which(param.dat$sig<0))  

tsb2.sig<-param.dat$sig
int.sig<-param.dat$sig
burnsev2.sig<-param.dat$sig
sigs<-cbind(tsb2.sig,int.sig,burnsev2.sig)
sigs<-as.data.frame(sigs)
sigs$all=rep(NA,nrow(sigs))

for (i in 1:nrow(sigs)){
  sigs$all[i]<-sum(sigs[i,1:3])
}

# Calculate covariate stats for Prediction Plots  ---------------------------------------------------------------------
# # (Figure 5)


year=as.factor(dat$Year)
prefire=dat$prefire
burnsev.raw=dat$burnsev
slope.raw=dat$slope
aspect.raw=dat$aspect
elevation.raw=dat$elevation
burnsev.raw=dat$burnsev
tree.raw=dat$TreeCov
snag.raw=dat$SnagBA
tsb.raw=dat$TSB
#standardize continuous covariates

burnsev.mean = mean(burnsev.raw,na.rm=T)
burnsev.sd = sd(burnsev.raw,na.rm=T)
burnsev = (burnsev.raw-burnsev.mean)/burnsev.sd

tsb.mean = mean(tsb.raw,na.rm=T)
tsb.sd = sd(tsb.raw,na.rm=T)
tsb = (tsb.raw-tsb.mean)/tsb.sd

slope.mean = mean(slope.raw,na.rm=T)
slope.sd = sd(slope.raw,na.rm=T)
slope= (slope.raw-slope.mean)/slope.sd

aspect.mean = mean(aspect.raw,na.rm=T)
aspect.sd = sd(aspect.raw,na.rm=T)
aspect= (aspect.raw-aspect.mean)/aspect.sd

elevation.mean = mean(elevation.raw,na.rm=T)
elevation.sd = sd(elevation.raw,na.rm=T)
elevation= (elevation.raw-elevation.mean)/elevation.sd

tree.mean = mean(tree.raw,na.rm=T)
tree.sd = sd(tree.raw,na.rm=T)
tree= (tree.raw-tree.mean)/tree.sd


snag.mean = mean(snag.raw,na.rm=T)
snag.sd = sd(snag.raw,na.rm=T)
snag= (snag.raw-snag.mean)/snag.sd



# Prediction Plots  ------------------------------------------------------------
results.predplot<-results.burn


PredPlot<-function(SPEC){
  max.tsb=15
  min.burnsev=0
  max.burnsev=100
  tsb.range = seq(1,15)
  burnsev.range = seq(min.burnsev,max.burnsev,,15)
  x <- (tsb.range - tsb.mean)/tsb.sd
  y <- (burnsev.range - burnsev.mean)/burnsev.sd
  z=matrix(NA,15,15)
  temp=NA
  temp2=NA
  
  #calculate predicted values and store in matrix z
  for (i in 1:15){
    for (j in 1:15){
      temp=which(results.predplot$SPEC==SPEC)
      temp2<-exp(results.predplot$prefirelate.closed[temp]+
                   (results.predplot$tsb[temp]*x[i]) +
                   (results.predplot$I.tsb.2.[temp]*(x[i]^2)) +
                   (results.predplot$burnsev[temp]*y[j]) +
                   (results.predplot$I.burnsev.2.[temp]*(y[j]^2)) +
                   (results.predplot$tsb.burnsev[temp]*x[i]*y[j]) )
      z[i,j]<-temp2 
    }} #end loops
  
  wireframe(z,
            rowvalues=tsb.range,
            column.values=burnsev.range,
            xlab="Time Since Fire",
            ylab="Burn Severity", 
            zlab=expression(atop(paste("Density"), "(#/ha)")),
            drape=T,
            colorkey=T,
            main=SPEC)
} #end function

WETA<-PredPlot("WETA")
DEJU<-PredPlot("DEJU") 
YRWA<-PredPlot("YRWA")
RBNU<-PredPlot("RBNU") 
FOSP<-PredPlot("FOSP") 
HEWA<-PredPlot("HEWA") 
MOCH<-PredPlot("MOCH") 
DUFL<-PredPlot("DUFL") 
LAZB<-PredPlot("LAZB") 
NAWA<-PredPlot("NAWA")
BRCR<-PredPlot("BRCR")
STJA<-PredPlot("STJA") 
WEWP<-PredPlot("WEWP")
HAWO<-PredPlot("HAWO")
TOSO<-PredPlot("TOSO")
MGWA<-PredPlot("MGWA") 
SPTO<-PredPlot("SPTO")
OSFL<-PredPlot("OSFL")
HAFL<-PredPlot("HAFL") 
CAFI<-PredPlot("CAFI")
CHSP<-PredPlot("CHSP")
AMRO<-PredPlot("AMRO") 
WHWO<-PredPlot("WHWO")
BHGR<-PredPlot("BHGR") 
CAVI<-PredPlot("CAVI")
GCKI<-PredPlot("GCKI") 
GTTO<-PredPlot("GTTO") 
HOWR<-PredPlot("HOWR") 
NOFL<-PredPlot("NOFL") 
HETH<-PredPlot("HETH")
WAVI<-PredPlot("WAVI")
MOUQ<-PredPlot("MOUQ") 
RBSA<-PredPlot("RBSA")
EVGR<-PredPlot("EVGR")
PISI<-PredPlot("PISI")
BBWO<-PredPlot("BBWO")
MOBL<-PredPlot("MOBL")
MODO<-PredPlot("MODO") 
WIWA<-PredPlot("WIWA")
CAHU<-PredPlot("CAHU")
PUFI<-PredPlot("PUFI")
WEBL<-PredPlot("WEBL")
WBNU<-PredPlot("WBNU")
GRFL<-PredPlot("GRFL")
BHCO<-PredPlot("BHCO") 
ANHU<-PredPlot("ANHU")
PIWO<-PredPlot("PIWO") 
LEGO<-PredPlot("LEGO")
WEFL<-PredPlot("WEFL") 
YEWA<-PredPlot("YEWA")
YEWA
WETA
DEJU 
YRWA
RBNU 
FOSP
HEWA
MOCH 
DUFL 
LAZB 
NAWA
BRCR
STJA
WEWP
HAWO
TOSO
MGWA 
SPTO
OSFL
HAFL 
CAFI
CHSP
AMRO 
WHWO
BHGR 
CAVI
GCKI 
GTTO 
HOWR 
NOFL 
HETH
WAVI
MOUQ 
RBSA
EVGR
PISI
BBWO
MOBL
MODO 
WIWA
CAHU
PUFI
WEBL
WBNU
GRFL
BHCO 
ANHU
PIWO 
LEGO
WEFL 


# build plot
tiff("figures/figure5.tiff",
     width=13,
     height=13,
     units="in",
     res=300,
     compression="lzw")
grid.arrange(OSFL,LAZB,HAWO,nrow=3,ncol=1)
dev.off()



