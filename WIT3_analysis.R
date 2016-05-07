library(dplyr)
library(ggplot2)
library(readxl)
library(magrittr)
library(lme4)
library(car)

dat = read.delim("../data/FullData.txt", stringsAsFactors=F)
IDsheet = read_excel("../data/IDsheet.xlsx")
laptop = read.table("../data/laptopInfo.txt", header=T)

# Fix up the IDsheet for clarity
IDsheet$Subset[IDsheet$Subset == "C"] = "Convenience"
IDsheet$Subset[is.na(IDsheet$Subset)] = "Inconvenience"

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

# Data exclusion for chance performance belongs here!
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
badSubs = meanAccTable %>%
  filter((meanACC < .608 & count == 120) | (meanACC < .615 & count==96)) %>%
  select(Subject) # doesn't matter whether p<.01 or p<.05

# analysis
datACC = dat %>%
  filter(feedbackmask == "fast") %>%
  filter(!(Subject %in% badSubs$Subject)) 

model1 = glmer(Probe.ACC ~ Condition * CueClass * Probe + (1|Subject), 
               family = "binomial",
               data=datACC)
Anova(model1, type=3)

# Well, do we replicate standard WIT effect?
model2 = 
datACC %>%
  filter(Condition == "GunTool") %>%
  glmer(Probe.ACC ~ CueClass * Probe + (1|Subject),
       family = "binomial",
       data = .)
Anova(model2, type=3)

# Restrict analysis to just gun trials per prereg
model3 = 
  datACC %>%
  filter(ProbeClass == "WEAP") %>%
  glmer(Probe.ACC ~ Condition * CueClass + (1|Subject),
        family = "binomial",
        data = .)
Anova(model3, type=3)
fixef(model3)

# Well what's our damn sample size like?
datACC  %>% distinct(Subject, Condition)  %$% table(Condition) # not great!

# Aggregate data for plotting
# Consider that some TrialTypes appear in both conditions 
  # Will this make group_by() and summarize() behave badly?
plotDatRT = dat %>%
  filter(Probe.ACC == 1, feedbackmask == "fast")  %>%  
  filter(!(Subject %in% badSubs$Subject)) %>%
  group_by(Condition, Subject, TrialType, Probe, CueClass) %>%
  summarize("meanRT" = mean(Probe.RT),
            "count" = n())
plotDatACC = dat %>%
  filter(feedbackmask == "fast") %>%
  filter(!(Subject %in% badSubs$Subject)) %>%
  group_by(Condition, Subject, TrialType, Probe, CueClass) %>%
  summarize("meanACC" = mean(Probe.ACC),
            "count" = n())

# Plots
ggplot(plotDatACC, aes(x=meanACC)) +
  geom_histogram()

ggplot(plotDatACC, aes(x=interaction(CueClass, Probe), y=meanACC)) +
  geom_point() +
  facet_wrap(~Condition)

plotDatACC %>%
  group_by(Condition, Probe, CueClass) %>%
  summarize("meanACC" = mean(meanACC)) %>%
  ggplot(., aes(x=interaction(CueClass, Probe), y=meanACC)) +
  geom_bar(stat="identity") +
  facet_wrap(~Condition)

plotDatACC %>%
  group_by(Condition, TrialType) %>%
  summarize("meanACC" = mean(meanACC)) %>%
  ggplot(., aes(x=TrialType, y=meanACC)) +
  geom_bar(stat="identity") +
  facet_wrap(~Condition)

ggplot(plotDatRT, aes(x=meanRT)) +
  geom_histogram()

ggplot(plotDatRT, aes(x=TrialType, y=meanRT)) +
  geom_point() +
  facet_wrap(~Condition)

plotDatRT %>%
  group_by(Condition, TrialType) %>%
  summarize("meanRT" = mean(meanRT)) %>%
  ggplot(., aes(x=TrialType, y=meanRT)) +
  geom_bar(stat="identity") +
  facet_wrap(~Condition)
