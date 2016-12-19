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
  glmer(Probe.ACC ~ Condition * Probe + 
          (1 + Condition * Probe|Subject), 
        data = ., family = "binomial")
summary(m1)
Anova(m1, type = 3)

# RT 
m1.rt = dat.rt %>% 
  filter(Cue == "hisp") %>% 
  lmer(Probe.RT ~ Condition * Probe + 
         (1 + Condition * Probe|Subject), data = .)
summary(m1.rt)
Anova(m1.rt, type = 3)

# each 2x2 interaction within levels of condition
# Accuracy
m2 = dat.acc %>% 
  filter(Condition == "BlackHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + 
          (1 + Cue * Probe|Subject), 
        data = ., family = "binomial")
summary(m2)
Anova(m2, type = 3)

m3 = dat.acc %>% 
  filter(Condition == "neutHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + 
          (1 + Cue * Probe|Subject), 
        data = ., family = "binomial")
summary(m3)
Anova(m3, type = 3)

m4 = dat.acc %>% 
  filter(Condition == "WhiteHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + 
          (1 + Cue * Probe|Subject), 
        data = ., family = "binomial")
summary(m4)
Anova(m4, type = 3)

# RT
m2 = dat.rt %>% 
  filter(Condition == "BlackHisp") %>% 
  lmer(Probe.rt ~ Cue * Probe + 
         (1 + Cue * Probe|Subject), 
        data = .)
summary(m2)
Anova(m2, type = 3)

m3 = dat.rt %>% 
  filter(Condition == "neutHisp") %>% 
  lmer(Probe.ACC ~ Cue * Probe + 
         (1 + Cue * Probe|Subject), 
        data = .)
summary(m3)
Anova(m3, type = 3)

m4 = dat.rt %>% 
  filter(Condition == "WhiteHisp") %>% 
  lmer(Probe.ACC ~ Cue * Probe + 
         (1 + Cue * Probe|Subject), 
        data = .)
summary(m4)
Anova(m4, type = 3)

# Plotting ---
dat.acc %>% 
  group_by(Subject, Condition, TrialType) %>% 
  summarize(Accuracy = mean(Probe.ACC, na.rm = T)) %>% 
  ggplot(aes(x = TrialType, y = Accuracy)) +
  geom_violin() +
  geom_boxplot(width = .3, notch = T) +
  facet_wrap(~Condition, scales = "free_x")

dat.rt %>% 
  group_by(Subject, Condition, TrialType) %>% 
  summarize(Latency = mean(Probe.RT, na.rm = T)) %>% 
  ggplot(aes(x = TrialType, y = Latency)) +
  geom_violin() +
  geom_boxplot(width = .3, notch = T) +
  facet_wrap(~Condition, scales = "free_x")

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