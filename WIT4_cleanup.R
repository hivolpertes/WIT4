library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)

# Data ingest ----
dat.raw = read.delim("./rawdata/WIT_study4_raw.txt")

# Discard excess E-Prime junk columns
dat = dat.raw %>% 
  select(Subject, Session,
         SessionDate, SessionTime, Condition, mapping,
         Trial, Gender.RESP, Race.RESP, SubTrial,
         BlackPrime, ChunkList, Correct, feedbackmask, GunTarget,
         ITIValue, NeuTarget, CueType,
         Probe.ACC, Probe.RT, Probe1.ACC, Probe1.RT,
         ProbeType, Procedure.Trial., Procedure.SubTrial., 
         Repeat, Running.SubTrial.,
         ToolTarget, TrialType, WhitePrime)
# Gender and Race information seem to persist nicely
table(dat$Subject, dat$Gender.RESP, useNA = 'always')
table(dat$Subject, dat$Race.RESP, useNA = 'always')
# Discard practice blocks and other nonsense
dat = dat %>% 
  filter(Procedure.Trial. == "RunProc") %>% 
  filter(Procedure.SubTrial. == "TrialProc")

# What's going on here? 
# Some have 120 rows, some have 240 rows, some have other # of rows
dat %>% 
  group_by(Subject, Condition, TrialType) %>% 
  summarize(length = n()) %>% 
  ungroup() %>% 
  select(-Subject) %>% 
  distinct()

# Looks like subjects in GunNeu condition did 240 trials, GunTool did 120
# That's a potential serious confound :-\
# Could try to address it by taking just the first 120 trials from GunNeu I guess

# Drop subjects Hannah tells me are rubbish
dat = dat %>% 
  filter(!(Subject %in% c(1, 2, 4, 5, 35)))

# Subjects 14, 15 were mistakenly run as 15, 16, leading to two IDs for 16
dat$Subject[dat$Subject == 15] <- 14
dat$Subject[dat$Subject == 16 & dat$Condition == "GunTool"] <- 15
# Hannah tells me Subject 58 was run as 57
dat$Subject[dat$Subject == 57 & dat$Condition == "GunNeu"] = 58
# Hannah tells me Subject 74 is two different people
# Can try to separate them on basis of time stamp, I guess
dat  %>% filter(Subject == 74)  %>% with(., table(Condition))
dat  %>% filter(Subject == 74)  %>% with(., table(Condition, SessionTime))
#15:28:42 vs 12:32:26
dat$Subject[dat$Subject == 74 & dat$SessionTime == "15:28:42"] = 110
# Hannah tells me Subject 77 is two different people
dat  %>% filter(Subject == 77)  %>% with(., table(Condition))
dat$Subject[dat$Subject == 77 & dat$Condition == "GunTool"] = 76

# Check N at start
dat %>% select(Subject, Condition) %>% distinct %>% 
  with(., table(Condition))

# Two subjects were run with previous experiment's Gun-Tool file
# Looks like they did 160 trials instead of usual 
dat %>% filter(Subject %in% c(34, 36)) %>% with(., table(Subject, Condition))
# Discard them for now but think about if they can/should be used
dat = dat %>% filter(!(Subject %in% c(34, 36)))

# TODO: Discard anybody not performing significantly above chance
mean.acc = dat %>% 
  group_by(Subject) %>% 
  summarize(accuracy = mean(Probe.ACC, na.rm = T))
# One subject seems to have reversed mapping. Consider bringing back in.
hist(mean.acc$accuracy)
# What's necessary accuracy for p < .01 above chance in 120 trials?
qbinom(.99, 120, .5, lower.tail = T)/120
qbinom(.95, 120, .5, lower.tail = T)/120
# Must be 60.8% accurate to ride
hist(mean.acc$accuracy[mean.acc$accuracy > .3])
abline(v = .608, col = 'red')
bad = mean.acc$Subject[mean.acc$accuracy < .608]
# Or 57.5% accurate?
bad.05 = mean.acc$Subject[mean.acc$accuracy < .575]

# Filter out bad-accuracy subjects
dat = dat %>% 
  filter(!(Subject %in% bad))

# TODO: Filter out Black subjects
# TODO: Ask Hannah what race is which code.
dat %>% 
  select(Subject, Race.RESP) %>% 
  distinct() %>% 
  group_by(Race.RESP) %>% 
  summarize(n = n())
dat = dat %>%
  filter(Race.RESP != 2)

# Final N
dat %>% 
  select(Subject, Condition) %>% 
  distinct() %>% 
  group_by(Condition) %>% 
  summarize(n = n())

write.table(dat, "clean_wit4.txt", sep = "\t", row.names = F)

# condense for repeated-measures ANOVA
dat.acc <- dat %>% 
  filter(feedbackmask == "fast") %>% 
  group_by(Subject, Session, Condition, mapping, TrialType) %>% 
  summarise(Probe.ACC = mean(Probe.ACC)) %>% 
  mutate(TrialType2 = TrialType) %>% 
  separate(TrialType2, into = c("Prime", "Target"), sep = 5)

dat.rt <- dat %>% 
  filter(Probe.ACC == 1) %>% 
  group_by(Subject, Session, Condition, mapping, TrialType) %>% 
  summarise(Probe.RT = mean(Probe.RT)) %>% 
  mutate(TrialType2 = TrialType) %>% 
  separate(TrialType2, into = c("Prime", "Target"), sep = 5)

write.table(dat.acc, "acc_wit4.txt", sep = "\t", row.names = F)
write.table(dat.rt, "rt_wit4.txt", sep = "\t", row.names = F)