# This script creates tables of means & SDs

setwd("C:/data_2014/Scherer_data_post/Study1")
dat = read.delim(file="WIT_study1_SASdat.txt")

# accuracy
(M1 = tapply(dat$acc, INDEX=list(dat$ExperimentName, dat$Cue, dat$Probe), FUN=mean))
(SD1 = tapply(dat$acc, INDEX=list(dat$ExperimentName, dat$Cue, dat$Probe), FUN=sd))

# RTs
(M2 = tapply(dat$rt, INDEX=list(dat$ExperimentName, dat$Cue, dat$Probe), FUN=mean))
(SD2 = tapply(dat$rt, INDEX=list(dat$ExperimentName, dat$Cue, dat$Probe), FUN=sd))

require(plyr)

write.table(adply(M1, 1:3), file="Study1_ACC_mean.txt", sep="\t", row.names=F)
write.table(adply(SD1, 1:3), file="Study1_ACC_SD.txt", sep="\t", row.names=F)
write.table(adply(M2, 1:3), file="Study1_RT_mean.txt", sep="\t", row.names=F)
write.table(adply(SD2, 1:3), file="Study1_RT_SD.txt", sep="\t", row.names=F)
