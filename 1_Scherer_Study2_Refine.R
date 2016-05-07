#NOTE: SCHERER_2013_D.TXT IS INSUFFICIENT, AS i WILL NEED THE SPECIFIC FACE.BMP INFORMATION
#I AM ALSO CURIOUS TO INSPECT THE DATE & TIME INFORMATION FOR SUBJECT 1

# Script 1:
# Scherer Study 2 Preprocessing
# This script deletes blank trials, 
# eliminates some redundant columns
# and appends subject race.

# after this, it's time for Script 2 to detect bad subjects

# Input is Scherer_2013_raw.txt
# Output is WIT_study2_refined.txt

setwd(choose.dir())
dat.orig=read.delim(file="WIT_study2_raw.txt", sep="\t")

#select the desired columns
keep=c("Subject", "Session", "mapping", "Procedure.Trial.", "Condition", "Trial",
       "TrialType", "CueType", "ProbeType", "Procedure.LogLevel5.",
       "feedduration", "Probe.ACC", "Probe.RT")
dat=dat.orig[,colnames(dat.orig) %in% keep]

#there are two subject 1s for some reason. The Session 22 sub 1 has no accuracy whatsoever. 
# We exclude that one
dat=subset(dat, dat$Session != 22)
# there's also a subject 883 that is probably me doing debug work
dat=subset(dat, dat$Subject != 883)

#things that don't belong in the data:
#Procedure.Trial. == "IntroProc"
#once this is accomplished, Procedure.Trial. can be removed.
#Condition == ""
#Trial == "NA"
#Procedure.LogLevel5. == "BreakTime" or "PrepProc"
#once this is accomplished, Procedure.LogLevel5. can be removed.
#DoIntro.Sample can be removed, as it is redundant with Block
#subject 883 is probably me and should be removed
dat=subset(dat, dat$Procedure.Trial. != "IntroProc" &
                dat$Condition != "" &
                dat$Trial != "NA" &
                dat$Procedure.LogLevel5. != "BreakTime" &
                dat$Procedure.LogLevel5. != "PrepProc" &
                dat$Subject != 883)
#name the variables to drop
drops=c("Session", "Procedure.Trial.", "ResponseMapping", "DoIntro.Sample", "Procedure.LogLevel5.")
dat=dat[,!(colnames(dat) %in% drops)] #and now drop 'em
#appending subject's race data
dat=data.frame(dat, "race"="white", stringsAsFactors=FALSE)
dat$race[dat$Subject %in% c(2,3,33,54,84) ] = "asian"
dat$race[dat$Subject %in% c(4,8,10,14,15,18,27,28,44,45,52,67,68,81,82)] = "african"
length(unique(dat$Subject)) 
colnames(dat)[colnames(dat)=="Subject"] = "sub" #renaming Subject column

#print("You must save manually, my lord.")
write.table(dat, file="WIT_study2_refined.txt", sep="\t", row.names=F)
