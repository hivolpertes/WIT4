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
# Note: Model currently failing to converge, gradient .004 instead of default .001
# Switching to Nelder_Mead optimizer did not fix that, nor did increasing maxfun to 2e5.
m1 = dat.acc %>% 
  filter(Cue == "hisp") %>% 
  glmer(Probe.ACC ~ Condition * Probe + 
          (1 + Probe|Subject), 
        data = ., family = "binomial",
        contrasts = list(Condition = "contr.sum",
                         Probe = "contr.sum"),
        control = glmerControl(optimizer = "Nelder_Mead",
                               optCtrl = list(maxfun = 20000)))
summary(m1)
Anova(m1, type = 3) # clear interaction

# RT 
# This converges nicely
m1.rt = dat.rt %>% 
  filter(Cue == "hisp") %>% 
  lmer(Probe.RT ~ Condition * Probe + 
         (1 + Probe|Subject),
       contrasts = list(Condition = "contr.sum",
                        Probe = "contr.sum"),
       data = .)
summary(m1.rt)
Anova(m1.rt, type = 3) # clear interaction

# each 2x2 interaction within levels of condition
# Accuracy
# This converges but rfx of subject and subject*probe are highly correlated
m2 = dat.acc %>% 
  filter(Condition == "BlackHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + 
          (1 + Probe|Subject), 
        contrasts = list(Probe = "contr.sum"),
        data = ., family = "binomial")
summary(m2)
Anova(m2, type = 3) # Very clear interaction

# Converges, everything's fine
m3 = dat.acc %>% 
  filter(Condition == "neutHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + 
          (1 + Probe|Subject), 
        contrasts = list(Probe = "contr.sum"),
        data = ., family = "binomial")
summary(m3)
Anova(m3, type = 3) # Very clear interaction

# Converges, rfx somewhat correlated r = .63
m4 = dat.acc %>% 
  filter(Condition == "WhiteHisp") %>% 
  glmer(Probe.ACC ~ Cue * Probe + 
          (1 + Probe|Subject),
        contrasts = list(Probe = "contr.sum"),
        data = ., family = "binomial")
summary(m4)
Anova(m4, type = 3) # no interaction, perhaps Ps can't tell White from Hispanic.

# RT
m2 = dat.rt %>% 
  filter(Condition == "BlackHisp") %>% 
  lmer(Probe.RT ~ Cue * Probe + 
         (1 + Probe|Subject),
       contrasts = list(Probe = "contr.sum"),
        data = .)
summary(m2)
Anova(m2, type = 3) # solid interaction

m3 = dat.rt %>% 
  filter(Condition == "neutHisp") %>% 
  lmer(Probe.RT ~ Cue * Probe + 
         (1 + Probe|Subject),
       contrasts = list(Probe = "contr.sum"),
        data = .)
summary(m3)
Anova(m3, type = 3) # very solid interaction

m4 = dat.rt %>% 
  filter(Condition == "WhiteHisp") %>% 
  lmer(Probe.RT ~ Cue * Probe + 
         (1 + Probe|Subject), 
       contrasts = list(Probe = "contr.sum"),
        data = .)
summary(m4)
Anova(m4, type = 3) # not-quite-there interaction

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

# Aggregation and table export
# Should I use conditional trials (dat.acc, dat.rt) or unconditional (dat)?
# I'll use conditional b/c that's what's actually analyzed.
# Another question is whether to first average w/in subjects 
# or just average across all trials directly.
# I think it's better to average w/in subjects first.
S2T1.acc = dat.acc %>% 
  group_by(Condition, TrialType, Subject) %>% 
  summarize(M.sub = mean(Probe.ACC, na.rm = T))%>% 
  summarize(M.acc = mean(M.sub, na.rm = T),
            SD.acc = sd(M.sub, na.rm = T))

S2T1.rt = dat.rt %>% 
  group_by(Condition, TrialType, Subject) %>% 
  summarize(M.sub = mean(Probe.RT, na.rm = T))%>% 
  summarize(M.rt = mean(M.sub, na.rm = T),
            SD.rt = sd(M.sub, na.rm = T))

S2T1 = full_join(S2T1.acc, S2T1.rt)
write.table(S2T1, "S2table.txt", sep = "\t", row.names = F)


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