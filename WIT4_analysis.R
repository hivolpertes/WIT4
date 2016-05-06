# TODO: Consider random effects modeling in greater detail.
#  Do I need random slopes of subject?
#  Do I need random effects of ProbeType and CueType?

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(BayesFactor)

# Data ingest ----
dat = read.delim("./clean_wit4.txt")
# WIT3 analysis seems to have coded for "CueClass" -- look into it
# Read the preregistration b/c I've kinda forgotten how this worked...
dat = dat %>% 
  mutate(Subject = as.factor(Subject),
         Congruency = ifelse(TrialType %in% c("BlackGun", "WhiteTool", "WhiteNeu"), "Congruent", "Incongruent"),
         ProbeClass = mapvalues(ProbeType,
                                from = c("absneut3.bmp", "banana.bmp", "giraffe.bmp", "gripstrength.bmp",
                                "TOOL1.bmp", "TOOL2.bmp", "TOOL3.bmp", "TOOL4.bmp",
                                "WEAPON1.bmp", "WEAPON2.bmp", "WEAPON3.bmp", "WEAPON4.bmp"),
                                to = rep(c("Neutral", "Tool", "Weapon"), each = 4)),
         CueClass = substr(CueType, 1, 5),
         Probe = ifelse(ProbeClass == "Weapon", "Weapon", "Other"),
         TrialClass = mapvalues(TrialType,
                                from = c("BlackGun", "BlackNeu", "BlackTool",
                                         "WhiteGun", "WhiteNeu", "WhiteTool"),
                                to = c("Black-Gun", "Black-Other", "Black-Other",
                                       "White-Gun", "White-Other", "White-Other")),
         TrialClass = factor(TrialClass, 
                             levels = c("Black-Gun", "Black-Other", "White-Gun", "White-Other"))
  )
# Convert factors to use contrast-coding for orthogonality
# dat = dat %>% 
#   mutate(Condition,
#          CueClass,
#          Probe)

# Make correct-only dataset for RT analyses
dat.rt = dat %>% 
  filter(Probe.ACC == 1)

# Make fast-only dataset for ACC analyses
dat.acc = dat %>% 
  filter(feedbackmask == "fast")

# Accuracy analyses ----
# Does accuracy depend on 3-way interaction?
mAcc = glmer(Probe.ACC ~ Condition * CueClass * Probe + (1 + CueClass * Probe|Subject) + 
               (1|ProbeType) + (1|CueType),
             data = dat.acc, family = "binomial",
             contrasts = list(Condition = contr.sum,
                              CueClass = contr.sum,
                              Probe = contr.sum))
summary(mAcc)

# Let's visualize the means
meanACC = dat.acc %>% 
  group_by(Condition, TrialClass) %>% 
  summarize(accuracy = mean(Probe.ACC, na.rm = T))
meanACC
ggplot(meanACC, aes(x = TrialClass, y = accuracy)) +
  geom_point() +
  facet_wrap(~Condition) +
  scale_y_continuous(limits = c(.6, .9))

# Okay, so hypothesis is that you find the same 
# (White-other - White-Gun) difference whether that "other" is
# tools or miscellaneous goofy objects
# This comes out great until you start adding random slopes or intercepts of Probe.
mAcc.GunNeu = dat %>% 
  filter(Condition == "GunNeu", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1 + Probe|Subject) + (1|ProbeType), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(mAcc.GunNeu) # Highly significant

mAcc.GunTool = dat %>% 
  filter(Condition == "GunTool", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1 + Probe|Subject) + (1|ProbeType), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(mAcc.GunTool) # Just significant, p = .039 if no random slope of Probe, n.s. with random slope of probe

# Bayes -- how much evidence do I have against 3-way interaction?
# Well, BayesFactor doesn't do binomial outcomes yet, does it now?

# RT analyses ----
mRT = lmer(Probe.RT ~ Condition * CueClass * Probe + (1|Subject),
            data = dat.rt,
           contrasts = list(Condition = contr.sum,
                            CueClass = contr.sum,
                            Probe = contr.sum))
summary(mRT)
Anova(mRT, type = 3) # uh, okay, so everything is wildly significant?

mRT2 = lmer(log(Probe.RT) ~ Condition * CueClass * Probe + (1|Subject),
           data = dat.rt)
summary(mRT2)
Anova(mRT2, type = 3)

meanRT = dat.rt %>% 
  group_by(Condition, TrialClass) %>% 
  summarise(RT = mean(Probe.RT, na.rm = T))
ggplot(meanRT, aes(x = TrialClass, y = RT)) +
  geom_point() +
  facet_wrap(~Condition)
# Faster to respond to White-Other than White-Gun... 
#  even after accounting for main effect of Target? hmmm.

# Is there a good way for me to check distribution of residuals?
# And is there a way to check on ICC?

# Is number of trials a factor?
exploratory1 = dat %>% 
  filter(Condition == "GunTool") %>% 
  glmer(Probe.ACC ~ TrialClass * SubTrial + (1 + SubTrial|Subject), 
                     data = ., family = "binomial")
summary(exploratory1) # One small just-significant interaction
exploratory2 = dat %>% 
  filter(Condition == "GunNeu") %>% 
  glmer(Probe.ACC ~ TrialClass * SubTrial + (1 + SubTrial|Subject), 
        data = ., family = "binomial")
summary(exploratory2) # looks like a significant fatigue effect

# Plotting playpen ----
dat.acc %>% 
  group_by(Subject, TrialClass, Condition) %>% 
  summarize(Accuracy = mean(Probe.ACC, na.rm = T)) %>% 
  ggplot(aes(x = TrialClass, y = Accuracy)) +
  facet_wrap(~Condition) +
  geom_violin() +
  geom_boxplot(width = .2, notch = T) +
  ggtitle("Violin & box plots of cell mean accuracy per subject")

dat.rt %>% 
  group_by(Subject, TrialClass, Condition) %>% 
  summarize(`Reaction Time` = mean(Probe.RT, na.rm = T)) %>% 
  ggplot(aes(x = TrialClass, y = `Reaction Time`)) +
  facet_wrap(~Condition) +
  geom_violin() +
  geom_boxplot(width = .2, notch = T) +
  ggtitle("Violin & box plots of cell mean RT per subject")
