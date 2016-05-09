# WIT study 1 analysis

library(plyr)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)

# data ingest
dat = read.delim("clean_wit1.txt", stringsAsFactors = F)

# add columns -- see 7_Study1_Sas-analysis
# this is still WIP, might not need it
dat = dat %>% 
  mutate(Condition = substring(ExperimentName, 5)) %>% # prune "WIT_" prefix
  mutate(ConditionCue = paste(Condition, Cue))

# TODO: Think carefully about these and double-check
# Original SAS script had "neutral_white White" and "neutral_white Neutral" flipped
gunPrimes = c("black_white Black", "neutral_black Black", "neutral_white White")
toolPrimes = c("black_white White", "neutral_black Neutral", "neutral_white Neutral")

dat = dat %>% 
  mutate(CueClass = ifelse(ConditionCue %in% gunPrimes, "Class_A", "Class_B"))

# Make fast-only dataset for accuracy
dat.acc = dat %>% 
  filter(Probe.RT <= 500)
# Make correct-only dataset for speed
dat.rt = dat %>% 
  filter(Probe.ACC == 1)

# Accuracy analysis
# Did I do this wrong? Takes forever to run on my laptop
# Consider role of random slopes.
# See if I can retrieve random stimulus data
m1 = glmer(Probe.ACC ~ Condition * CueClass * Probe + (1|Subject),
          data = dat.acc, family = "binomial")
summary(m1)
Anova(m1, type = 3)

# 2x2s within each level of Condition
m2 = dat.acc %>% 
  filter(Condition == "black_white") %>% 
  glmer(Probe.ACC ~ Cue * Probe + (1|Subject),
           data = ., family = "binomial")
summary(m2)
Anova(m2, type = 3)

m3 = dat.acc %>% 
  filter(Condition == "neutral_black") %>% 
  glmer(Probe.ACC ~ Cue * Probe + (1|Subject),
        data = ., family = "binomial")
summary(m3)
Anova(m3, type = 3)

m4 = dat.acc %>% 
  filter(Condition == "neutral_white") %>% 
  glmer(Probe.ACC ~ Cue * Probe + (1|Subject),
        data = ., family = "binomial")
summary(m4)
Anova(m4, type = 3)

# Could yet implement a cell means model, too.

# Another way of implementing it is to condition on Prime
#  and look for the interactions with Condition

pm1 = dat.acc %>% 
  filter(Cue == "Black") %>% 
  glmer(Probe.ACC ~ Condition * Probe + (1|Subject),
        data = ., family = "binomial")
summary(pm1)
Anova(pm1, type = 3)

pm2 = dat.acc %>% 
  filter(Cue == "White") %>% 
  glmer(Probe.ACC ~ Condition * Probe + (1|Subject),
        data = ., family = "binomial")
summary(pm2)
Anova(pm2, type = 3) # Note the big flip

pm3 = dat.acc %>% 
  filter(Cue == "Neutral") %>% 
  glmer(Probe.ACC ~ Condition * Probe + (1|Subject),
        data = ., family = "binomial")
summary(pm3)
Anova(pm3, type = 3) # Again a big flip

# Reaction time analyses

# Condition on Prime and look for the interactions with Condition

pm1 = dat.rt %>% 
  filter(Cue == "Black") %>% 
  lmer(Probe.RT ~ Condition * Probe + (1|Subject),
        data = .)
summary(pm1)
Anova(pm1, type = 3)

pm2 = dat.rt %>% 
  filter(Cue == "White") %>% 
  lmer(Probe.RT ~ Condition * Probe + (1|Subject),
        data = .)
summary(pm2)
Anova(pm2, type = 3) # Note the big flip

pm3 = dat.rt %>% 
  filter(Cue == "Neutral") %>% 
  lmer(Probe.RT ~ Condition * Probe + (1|Subject),
        data = .)
summary(pm3)
Anova(pm3, type = 3) # Again a big flip

# Now the trick will be formalizing this in some sort of effect size...