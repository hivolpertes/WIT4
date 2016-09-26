library(dplyr)
library(ggplot2)
library(readxl)
library(magrittr)
library(lme4)
library(car)

dat = read.delim("clean_wit3.txt", stringsAsFactors = F)
# convert subject to factor
dat$Subject = as.factor(dat$Subject)
# Check N
dat %>% 
  select(Subject, Condition, Subset) %>% 
  distinct %>% 
  with(., table(Condition, Subset))

# analysis
dat.acc = dat %>%
  filter(feedbackmask == "fast")
dat.rt = dat %>% 
  filter(Probe.ACC == 1)

# Model fails to converge, max|grad| = .023 vs tol .001
# Note highly correlated random slopes!
model1 = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
                 (1 + CueClass * Probe|Subject), 
               contrasts = list(Condition = "contr.sum",
                                CueClass = "contr.sum",
                                Probe = "contr.sum"),
               family = "binomial",
               data=dat.acc)
summary(model1)
Anova(model1, type=3)

model1a = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
                 (1 + CueClass + Probe|Subject), 
               contrasts = list(Condition = "contr.sum",
                                CueClass = "contr.sum",
                                Probe = "contr.sum"),
               family = "binomial",
               data=dat.acc)
summary(model1a)
Anova(model1a, type=3)

# Well, do we replicate standard WIT effect?
# Convergence a little rough, max|grad| = .0015 vs tol = .001
# Again notice terribly high correlation of random slope parameters
model2gt = 
  dat.acc %>%
  filter(Condition == "GunTool") %>%
  glmer(Probe.ACC ~ CueClass * Probe + (1 + CueClass * Probe|Subject),
        family = "binomial",
        contrasts = list(CueClass = "contr.sum",
                         Probe = "contr.sum"),
        data = .)
summary(model2gt)
Anova(model2gt, type=3) # p = .055

# What about in the novel task?
# Negative eigenvalues if modeling random slopes of Cue-Probe interaction
model2bg = dat.acc %>%
  filter(Condition == "BlackGun") %>%
  glmer(Probe.ACC ~ CueClass * Probe + (1 + CueClass + Probe|Subject),
        family = "binomial",
        contrasts = list(CueClass = "contr.sum",
                         Probe = "contr.sum"),
        data = .)
summary(model2bg)
Anova(model2bg, type=3) # p = .025

# Restrict analysis to just gun trials per prereg
model3 = 
  dat.acc %>%
  filter(ProbeClass == "WEAP") %>%
  glmer(Probe.ACC ~ Condition * CueClass + 
          (1 + CueClass|Subject),
        family = "binomial",
        contrasts = list(CueClass = "contr.sum"),
        data = .)
summary(model3)
Anova(model3, type=3) # null result, p = .15
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
plotDatACC %>%
  ggplot(., aes(x=interaction(CueClass, Probe), y=meanACC)) +
  #geom_violin() +
  geom_boxplot(width = .2, notch = T) +
  facet_wrap(~Condition)

plotDatACC %>%
  group_by(Condition, TrialType) %>%
  summarize("meanACC" = mean(meanACC)) %>%
  ggplot(., aes(x=TrialType, y=meanACC)) +
  geom_bar(stat="identity") +
  facet_wrap(~Condition)


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


