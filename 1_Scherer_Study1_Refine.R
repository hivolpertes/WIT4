# #Scherer Study 1 Cleaning
# #This script deletes blank trials, 
# breaks the subjects into their individual experiments,
# and appends subject race.
# Curiously, 15 subjects seem to be missing.
# Input is WIT_study1_raw.txt
# Output is WIT_study1_refined.txt

setwd("C:/data_2014/Scherer_data_post/Study1")
dat=read.delim(file="WIT_study1_raw.txt")
dat=subset(dat, dat$SubTrial != "NA") #removing these weird trials
dat=subset(dat, dat$Probe.ACC != "NA") #removing these breaks
dat=dat[,colnames(dat) != "ChunkList.Sample"] #remove column Chunklist.Sample (redundant)
#let's break subjects out by condition.
tapply(dat$Subject, dat$ExperimentName, FUN=max)
#"Condition 1" is black & white
#"Condition 2" is neutral & black
#"Condition 3" is neutral & white. Thus,
dat = data.frame( "sub" = dat$Subject, dat, "race" = factor("white", levels=c("white", "minority"))) #add columns
exp2 = (dat$ExperimentName == "WIT_neutral_black")
exp3 = (dat$ExperimentName == "WIT_neutral_white")
dat$sub[exp2] = dat$Subject[exp2] + 100
dat$sub[exp3] = dat$Subject[exp3] + 200
minority = c( 11, 12, 14, 15, 16, 20, 23, 29, 30, 31, 33, 34, 38, 39, 40, 47, 48,
              100+c(4, 15, 17, 32, 37, 40, 41, 43, 45),
              200+c(6, 14, 18, 20, 28, 32, 34) 
)
dat$race[dat$sub %in% minority] = "minority"


length(levels(as.factor(dat$sub))) #121 subjects < (50 + 45 + 41 = 136)
#where are we missing dudes?
tapply(dat$sub, dat$sub, FUN=length) #we seem to be missing some subjects for some reason?
missing = c(7:9, 11:12, 34, 106:107, 111:112, 206:207, 210:212)
length(missing) #the mysteriously absent 15 subjects...
#We'll figure out the missing subjects later, I guess. 
#Let's see what the analysis looks like without them

#print("You must save manually, my lord.")
write.table(dat, file="WIT_study1_refined.txt", sep="\t", row.names=F)
