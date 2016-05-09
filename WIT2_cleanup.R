#NOTE: SCHERER_2013_D.TXT IS INSUFFICIENT, AS i WILL NEED THE SPECIFIC FACE.BMP INFORMATION
#  I AM ALSO CURIOUS TO INSPECT THE DATE & TIME INFORMATION FOR SUBJECT 1

# Script 1:
# Scherer Study 2 Preprocessing
# This script deletes blank trials, 
# eliminates some redundant columns
# and appends subject race.

# It also deletes participants with
# accuracy not sig. above chance

# Input is Scherer_2013_raw.txt
# Output is WIT_study2_refined.txt

dat = read.delim(file="./rawdata/WIT_study2_raw.txt", sep="\t")

#select the desired columns
dat = dat %>% 
  select(Subject, Session, mapping, Procedure.Trial., Condition, Trial,
         TrialType, CueType, ProbeType, Procedure.LogLevel5.,
         feedduration, Probe.ACC, Probe.RT)

# Drop two bad subject-files: Subject 1 session 22 has no accuracy,
#  and Subject 883 is probably me doing debug work
dat = dat %>% 
  filter(Session != 22,
         Subject != 883)

#things that don't belong in the data:
#Procedure.Trial. == "IntroProc"
#once this is accomplished, Procedure.Trial. can be removed.
#Condition == ""
#Trial == "NA"
#Procedure.LogLevel5. == "BreakTime" or "PrepProc"
#once this is accomplished, Procedure.LogLevel5. can be removed.
#DoIntro.Sample can be removed, as it is redundant with Block
#subject 883 is probably me and should be removed
dat = dat %>% 
  filter(Procedure.Trial. != "IntroProc",
         Condition != "",
         !is.na(Trial),
         !Procedure.LogLevel5. %in% c("BreakTime", "PrepProc"))

# drop further unnecessary columns
dat = dat %>% 
  select(-Session, -Procedure.Trial., -Procedure.LogLevel5.)
# appending subject's race data
dat = data.frame(dat, "race"="white", stringsAsFactors=FALSE)
dat$race[dat$Subject %in% c(2,3,33,54,84) ] = "asian"
dat$race[dat$Subject %in% c(4,8,10,14,15,18,27,28,44,45,52,67,68,81,82)] = "african"
length(unique(dat$Subject)) 

# Exclude those participants who are not significantly better than chance
# not sure whether alpha = .05 or alpha = .01 (or any difference)

mean.acc = dat %>% 
  group_by(Subject) %>% 
  summarize(accuracy = mean(Probe.ACC, na.rm = T),
            trials = n())
# Are subjects with <20% accuracy performing reversed mapping, 
#  or are they not responding?
hist(mean.acc$accuracy)
# What's necessary accuracy for p < .01 above chance in 256 trials?
qbinom(.99, 288, .5, lower.tail = T)/288
# Must be 56.9% accurate to ride
hist(mean.acc$accuracy)
abline(v = .569, col = 'red')
bad = mean.acc$Subject[mean.acc$accuracy < .569]

# Filter out bad-accuracy subjects
dat = dat %>% 
  filter(!(Subject %in% bad))
# Filter out black participants
dat = dat %>% 
  filter(!(race == "african"))

write.table(dat, file="clean_wit2.txt", sep="\t", row.names=F)
