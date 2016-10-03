library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(magrittr)
library(lme4)
library(car)

dat = read.delim("./rawdata/WIT_study3_raw.txt", stringsAsFactors=F)
IDsheet = read_excel("./rawdata/WIT_study3_IDsheet.xlsx")
laptop = read.table("./rawdata/WIT_study3_laptopInfo.txt", header=T)

# Fix up the IDsheet for clarity
IDsheet$Subset[IDsheet$Subset == "C"] = "Convenience"
IDsheet$Subset[is.na(IDsheet$Subset)] = "Inconvenience"

# Check pre-exclusion N
with(IDsheet, table(Cond, Subset))
dat %>% 
  select(Subject, Condition) %>% 
  distinct() %>% 
  with(., table(Condition))
# WARNING: THESE DON'T MATCH

# Join the data frames
dat = left_join(dat, IDsheet)
dat = left_join(dat, laptop)

# Data tidying
dat = dat %>%
  # Select and rename relevant columns
  select(
    # Session & Subject ID info
    Subject, "Script" = ExperimentName, Condition, 
    "Gender" = Gender.RESP.Block., Sex, "Race" = Race.RESP.Block., Subset,
    mapping, laptop, "Date" = SessionDate, "Time" = SessionTime,
    # Block & Trial number info
    "Block" = Procedure.Trial., "Trial" = SubTrial, 
    # Trial type & Stimulus info
    TrialType, CueType, ProbeType, BlackPrime, BlackTarget, GunTarget, WhitePrime, ToolTarget, 
    # Accuracy and RT info
    Probe.ACC, Probe.RT, feedbackmask,
    Note
  ) %>%
  # Discard practice rows and breaktime rows  
  filter(Block == "RunProc", TrialType != "NULL") %>%
  # Discard African-American subjects per preregistration
  filter(Race != 2) %>%
  # Convert ACC and RT back to numeric
  mutate("Probe.ACC" = as.numeric(Probe.ACC), "Probe.RT" = as.numeric(Probe.RT)) %>%
  # Convert Subject number to factor
  mutate("Subject" = as.factor(Subject)) %>%
  # Add simpler codes for Cue and Probe
  mutate("CueClass" = substr(CueType, 1, 5),
         "ProbeClass" = substr(ProbeType, 1, 4)) %>%
  mutate("Probe" = ifelse(ProbeClass %in% c("Blac", "TOOL"), "Other", "Gun"))

# Check trial counts
table(dat$Subject, dat$TrialType)

# # Investigate Subject 1 -- probably two subjects given same ID by accident
# dat %>%
#   filter(Subject == 1) %>%
#   View
# Judging by the date and the number of trials, that must have been me included by accident. Removing.
dat = dat %>%
  filter(!(Subject == 1 & Date == "5/4/2015"))
# Check trial counts again
table(dat$Subject, dat$TrialType) # Good!

# Remove the two subjects who weren't paying attention
dat = dat %>%
  filter(!(Note %in% "BAD"))

# Count number of too-slows:
temp = dat %>%
  filter(feedbackmask == "slow") %>%
  group_by(Subject) %>%
  summarize("count" = n()) 
as.data.frame(temp)
ggplot(temp, aes(x=count)) + geom_histogram()
# Count number of fast trials:
temp = dat %>%
  filter(feedbackmask == "fast") %>%
  group_by(Subject) %>%
  summarize("count" = n()) 
as.data.frame(temp)
ggplot(temp, aes(x=count)) + geom_histogram()

# What's significant (p<.05) accuracy in this task?
qbinom(.95, 30*4, .5) # 69 / 120 = 57.5%
qbinom(.95, 24*4, .5) # 56 / 96 = 58.3%
# What's significant (p<.01) accuracy in this task?
qbinom(.99, 30*4, .5) # 73 / 120 = 60.8%
qbinom(.99, 24*4, .5) # 59 / 96 = 61.5%
# get mean accuracies
meanAccTable = dat %>%
  group_by(Subject) %>%
  summarize("meanACC" = mean(Probe.ACC),
            "count" = n())
# Filter out subjects that don't demonstrate accuracy sig. greater than chance,
  # p < .01
badSubs = meanAccTable %>%
  filter((meanACC < .608 & count == 120) | (meanACC < .615 & count==96)) %>%
  select(Subject) # doesn't matter whether p<.01 or p<.05
dat = dat %>% 
  filter(!(Subject %in% badSubs))

write.table(dat, "clean_wit3.txt", sep = "\t", row.names = F)

dat.acc <- dat %>% 
  filter(feedbackmask == "fast") %>% 
  group_by(Subject, Condition, mapping, TrialType, CueClass, ProbeClass) %>% 
  summarize(Probe.ACC = mean(Probe.ACC))

write.table(dat.acc, file = "acc_wit3.txt", sep = "\t", row.names = F)

dat.rt <- dat %>% 
  filter(Probe.ACC == 1) %>% 
  group_by(Subject, Condition, mapping, TrialType, CueClass, ProbeClass) %>% 
  summarize(Probe.RT = mean(Probe.RT))

write.table(dat.rt, file = "rt_wit3.txt", sep = "\t", row.names = F)