# WIT study 2 analysis

# TODO: Remember to perform sensitity analysis dropping Zimmerman prime

library(plyr)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)

# data ingest
dat = read.delim("clean_wit2.txt")

# make "not-hispanic" prime category

# Consider making "CueSort" per SAS code at bottom
dat = dat %>% 
  mutate(Cue = substring(CueType, 1, 4),
         Probe = substring(ProbeType, 1, 4),
         CueClass = ifelse(TrialType %in% c("HispGun", "HispTool"), "Hisp", "NonHisp"))

# make accuracy and RT datasets
dat.acc = dat %>% 
  filter(Probe.RT <= 500)
dat.rt = dat %>% 
  filter(Probe.ACC == 1)

# 2x3 interaction where Prime == hisp
# Accuracy
m1 = dat.acc %>% 
  filter(Cue == "hisp") %>% 
  glmer(Probe.ACC ~ Condition * Probe + (1|Subject), 
        data = ., family = "binomial")
summary(m1)
Anova(m1, type = 3)

# RT 
m1.rt = dat.rt %>% 
  filter(Cue == "hisp") %>% 
  lmer(Probe.RT ~ Condition * Probe + (1|Subject), data = .)
summary(m1.rt)
Anova(m1.rt)

# each 2x2 interaction within levels of condition
m2 = dat.acc %>% 
  filter(Condition == "BlackHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + (1|Subject), 
        data = ., family = "binomial")
summary(m2)
Anova(m2)

m3 = dat.acc %>% 
  filter(Condition == "neutHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + (1|Subject), 
        data = ., family = "binomial")
summary(m3)
Anova(m3)

m4 = dat.acc %>% 
  filter(Condition == "WhiteHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + (1|Subject), 
        data = ., family = "binomial")
summary(m4)
Anova(m4)

# SAS code graveyard ----
# data dat_2; set dat;
# TrialSort = TrialType;
# if TrialType in ("BlackGun", "WhiteGun", "NeutGun", "WhiteGun") then TrialSort = "OtherGun";
# if TrialType in ("BlackTool", "WhiteTool", "NeutTool", "WhiteTool") then TrialSort = "OtherTool";
# if trialsort in ("HispTool", "HispGun") then CueSort = "Hispanic";
# if trialsort in ("OtherTool", "OtherGun") then CueSort = "Other";
# run; 
# data dat_nozim2; set dat_nozim;
# TrialSort = TrialType;
# if TrialType in ("BlackGun", "WhiteGun", "NeutGun", "WhiteGun") then TrialSort = "OtherGun";
# if TrialType in ("BlackTool", "WhiteTool", "NeutTool", "WhiteTool") then TrialSort = "OtherTool";
# if trialsort in ("HispTool", "HispGun") then CueSort = "Hispanic";
# if trialsort in ("OtherTool", "OtherGun") then CueSort = "Other";