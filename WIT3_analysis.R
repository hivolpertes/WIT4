library(dplyr)
library(ggplot2)
library(readxl)
library(magrittr)
library(lme4)
library(car)

dat = read.delim("clean_wit3.txt", stringsAsFactors = F)
# convert subject to factor
dat$Subject = as.factor(dat$Subject)

# analysis
datACC = dat %>%
  filter(feedbackmask == "fast")

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
Anova(model2, type=3) # p = .081

# Restrict analysis to just gun trials per prereg
model3 = 
  datACC %>%
  filter(ProbeClass == "WEAP") %>%
  glmer(Probe.ACC ~ Condition * CueClass + (1|Subject),
        family = "binomial",
        data = .)
Anova(model3, type=3) # null result, although seems like fairly ambiguous result
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
# TODO: violins or something might be nicer
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
