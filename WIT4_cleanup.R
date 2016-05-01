library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)

# Data ingest ----
dat.raw = read.delim("./rawdata/Merge105.txt")

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
sum1 = dat %>% 
  group_by(Subject, Condition) %>% 
  summarize(length = length(Session))
table(sum1$Condition, sum1$length)
# Looks like subjects in GunNeu condition did 240 trials, GunTool did 120
# That's a potential serious confound :-\
# Could try to address it by taking just the first 120 trials from GunNeu I guess

# Drop subjects Hannah tells me are rubbish
dat = dat %>% 
  filter(!(Subject %in% c(1, 2, 4, 5, 35)))

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
# Must be 60.8% accurate to ride
hist(mean.acc$accuracy[mean.acc$accuracy > .3])
abline(v = .608, col = 'red')
bad = mean.acc$Subject[mean.acc$accuracy < .608]

# Filter out bad-accuracy subjects
dat = dat %>% 
  filter(!(Subject %in% bad))

# TODO: Filter out Black subjects
# TODO: Ask Hannah what race is which code.
dat = dat %>%
  filter(Race.RESP != 2)

write.table(dat, "clean_wit4.txt", sep = "\t", row.names = F)