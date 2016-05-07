#Scherer Study 1 data aggregation
#step 3: create tables of grouped correct-trial RT and overall accuracy

#Required data:
#Data in: "WIT_study1_refined.txt" from 1_Scherer_Study1_Refine.R
#Data in: "WIT_study1_badsubs.txt" from 2_Scherer_Study2_BadSubs.R

library(reshape2)
setwd("C:/data_2014/Scherer_data_post/Study1")

dat.orig=read.delim(file="WIT_study1_refined.txt", sep="\t")
bad=scan(file="WIT_study1_badsubs.txt", sep="\t")
dat=dat.orig[!(dat.orig$sub %in% bad),]  #exclude bad subjects
dat=dat[dat$race == "white",]        #exclude minority subjects

molten = melt(dat, 
          id.var=c("sub", "Subject", "ExperimentName", "mapping", "SubTrial", "feedbackmask",
                   "feedduration", "ITIList", "ITIValue", "TrialType", "Cue", "Probe", "race"
                   )
              ) 

d = dcast(molten, sub + race + ExperimentName + TrialType + mapping + Cue + Probe ~ variable, 
         fun.aggregate=mean)

hist(d$Probe.RT)
plot(density(d$Probe.RT))
hist(d$Probe.ACC)
plot(density(d$Probe.ACC))

#RT analyses
c.dat=subset(dat, dat$Probe.ACC == 1)
c.molten = melt(c.dat, 
                id.var=c("sub", "Subject", "ExperimentName", "mapping", "SubTrial", 
                         "feedbackmask", "feedduration", "ITIList", "ITIValue", "TrialType", 
                         "Cue", "Probe", "race"
                        )
                )
c.d = dcast(c.molten, sub + ExperimentName + TrialType + mapping + Cue + Probe ~ variable, 
           fun.aggregate=mean)
#View(c.d) #whats the story with these "NA" rows???
plot(density(c.d$Probe.RT))

#put together the accuracy data with the RT data:
export=data.frame(c.d, "acc"=d$Probe.ACC); 
export=export[,-c(7)]; colnames(export)[7]="rt";

#you may now export the combined data
#print("Thou must save manually, my Lord!")
write.table(export, file="WIT_study1_SASdat.txt", sep="\t", row.names=F) 
