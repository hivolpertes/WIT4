# This script creates tables of means & SDs

setwd(choose.dir())
dat = read.delim(file="WIT_study2_SASdat.txt")
dat$CueClass = "Hispanic"
dat$CueClass[dat$CueType != "hispanic"] = "Context"

# accuracy
(M1 = tapply(dat$acc, INDEX=list(dat$Condition, dat$CueClass, dat$ProbeType), FUN=mean))
(SD1 = tapply(dat$acc, INDEX=list(dat$Condition, dat$CueClass, dat$ProbeType), FUN=sd))

# RTs
(M2 = tapply(dat$rt, INDEX=list(dat$Condition, dat$CueClass, dat$ProbeType), FUN=mean))
(SD2 = tapply(dat$rt, INDEX=list(dat$Condition, dat$CueClass, dat$ProbeType), FUN=sd))

require(plyr)

write.table(adply(M1, 1:3), file="Study2_ACC_mean.txt", sep="\t", row.names=F)
write.table(adply(SD1, 1:3), file="Study2_ACC_SD.txt", sep="\t", row.names=F)
write.table(adply(M2, 1:3), file="Study2_RT_mean.txt", sep="\t", row.names=F)
write.table(adply(SD2, 1:3), file="Study2_RT_SD.txt", sep="\t", row.names=F)
