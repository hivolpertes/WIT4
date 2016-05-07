#Scherer Study 2 data aggregation
#step 3: create tables of grouped correct-trial RT and overall accuracy

#Required data:
#Data in: "WIT_study2_refined.txt" from 1_Scherer_Study2_Refine.R
#Data in: "WIT_study2_badsubs_2.txt" from 2_Scherer_Study2_BadSubs.R

library(reshape2)
setwd(choose.dir())

dat.orig=read.delim(file="WIT_study2_refined.txt", sep="\t", stringsAsFactors=F)
bad=scan(file="WIT_study2_badsubs_2.txt", sep="\t")
dat=dat.orig[!(dat.orig$sub %in% bad),]  #exclude bad subjects
dat=dat[dat$race == "white",]        #exclude minority subjects
#dat=subset(dat, CueType != "hispFace3.bmp") #exclude Zimmerman prime

####################################
#Collapsing stimuli within a set...#
####################################

h=c("hispFace1.bmp", "hispFace2.bmp", "hispFace3.bmp", "hispFace4.bmp")
b=c("BlackFace1.bmp", "BlackFace2.bmp", "BlackFace3.bmp", "BlackFace4.bmp")
w=c("whiteface1.bmp", "whiteface2.bmp", "whiteface3.bmp", "whiteface4.bmp")
a=c("absneut1.BMP", "absneut2.BMP", "absneut3.BMP", "absneut4.BMP")
tool=c("TOOL1.bmp", "TOOL2.bmp", "TOOL3.bmp", "TOOL4.bmp")
weapon=c("WEAPON1.bmp", "WEAPON2.bmp", "WEAPON3.bmp", "WEAPON4.bmp")
dat$CueType[dat$CueType %in% h] = "hispanic"
dat$CueType[dat$CueType %in% b] = "black"
dat$CueType[dat$CueType %in% w] = "white"
dat$CueType[dat$CueType %in% a] = "abstract"
dat$ProbeType[dat$ProbeType %in% tool] = "tool"
dat$ProbeType[dat$ProbeType %in% weapon] = "weapon"
######################
#Finished Collapsing!#
#Begin Melt & Cast...#
######################


molten = melt(dat, 
              id.var=c("sub", "mapping", "Condition", "Trial", "CueType", "ProbeType",
                       "feedduration", "TrialType", "race"
              )
) 

d = dcast(molten, sub + race + Condition + TrialType + CueType + ProbeType + mapping ~ variable, 
         fun.aggregate=mean)

hist(d$Probe.RT)
plot(density(d$Probe.RT))
hist(d$Probe.ACC)
plot(density(d$Probe.ACC))

#RT analyses
c.dat=subset(dat, dat$Probe.ACC == 1)
c.molten = melt(c.dat, 
                id.var=c("sub", "mapping", "Condition", "Trial", "CueType", "ProbeType",
                         "feedduration", "TrialType", "race"
                )
)
c.d = dcast(molten, sub + race + Condition + TrialType + CueType + ProbeType + mapping ~ variable, 
           fun.aggregate=mean)
plot(density(c.d$Probe.RT))

#put together the accuracy data with the RT data:
export=data.frame(c.d, "acc"=d$Probe.ACC); 
drop="Probe.ACC"; export=export[!(colnames(export) %in% drop)] #drop redundant Probe.ACC column
colnames(export)[colnames(export)=="Probe.RT"]="rt"; #rename "Probe.RT" to simpler "rt"

#you may now export the combined data
#print("Thou must save manually, my Lord!")
write.table(export, file="WIT_study2_SASdat.txt", sep="\t", row.names=F) 