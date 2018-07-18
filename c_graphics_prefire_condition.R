# Similar to "c_graphics.R" but only builds figure 6 (prefire condition)

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



# prepare data -----------------------------------------------------

results.in=results.burn
# get coefficient values
prefire<-data.frame(results.in$SPEC,
                    results.in$prefireopen,
                    results.in$prefireShrub,
                    results.in$prefiremid.closed,
                    results.in$prefireyoung.forest,
                    results.in$prefirelate.closed,
                    results.in$lower.prefireopen,
                    results.in$lower.prefireShrub,
                    results.in$lower.prefiremid.closed,
                    results.in$lower.prefireyoung.forest,
                    results.in$lower.prefirelate.closed,
                    results.in$upper.prefireopen,
                    results.in$upper.prefireShrub,
                    results.in$upper.prefiremid.closed,
                    results.in$upper.prefireyoung.forest,
                    results.in$upper.prefirelate.closed)
colnames(prefire)<-c("spec",
                     "open",
                     "shrub",
                     "mid.closed",
                     "young.forest",
                     "late.closed",
                     "lower.open",
                     "lower.shrub",
                     "lower.mid.closed",
                     "lower.young.forest",
                     "lower.late.closed",
                     "upper.open",
                     "upper.shrub",
                     "upper.mid.closed",
                     "upper.young.forest",
                     "upper.late.closed"
)

#get extract desired species and severity
#prefire<-prefire[c(
#  which(prefire$spec=="FOSP"),
 # which(prefire$spec=="GTTO"),
 # which(prefire$spec=="LAZB"),
 # which(prefire$spec=="MGWA"),
 # which(prefire$spec=="SPTO")),]


# melt into long format for ggplot2
prefire.melt1<-melt(prefire,id.vars="spec",
                    measure.vars=c("open","shrub","mid.closed","young.forest","late.closed"),
                    value.name="beta")
prefire.melt2<-melt(prefire,id.vars="spec",
                    measure.vars=c("lower.open","lower.shrub","lower.mid.closed","lower.young.forest","lower.late.closed"),
                    value.name="lower")
prefire.melt3<-melt(prefire,id.vars="spec",
                    measure.vars=c("upper.open","upper.shrub","upper.mid.closed","upper.young.forest","upper.late.closed"),
                    value.name="upper")

prefire.melt<-data.frame(spec=prefire.melt1$spec,
                         Prefire_Condition=prefire.melt1$variable,
                         value=exp(prefire.melt1$value),
                         lower=exp(prefire.melt2$value),
                         upper=exp(prefire.melt3$value))

prefire.melt$Prefire_Condition<-factor(prefire.melt$Prefire_Condition,levels=c("shrub",
                                                                               "young.forest",
                                                                               "open",
                                                                               "mid.closed",
                                                                               "late.closed"))

# prepare space to write plot
tiff("figures/figure6.tiff",
     width=5,
     height=4,
     units="in",
     res=300,
     compression="lzw")

# calculate max for plot
axis.max<-max(prefire.melt$upper)
prefire.plot<-ggplot(prefire.melt,
                     aes(x=factor(spec), 
                         y=value,
                         ymin=lower,
                         ymax=upper))

# build plot
prefire.plot + 
  geom_pointrange(aes(color=Prefire_Condition),position=position_dodge(width=.4)) +
  ylim(0,axis.max)+
  coord_flip() +
  xlab("Species")+
  ylab("Mean Predicted Density (#/ha)")+
  scale_fill_discrete(name="Prefire Condition")+
  ggtitle("High Severity")
dev.off()



