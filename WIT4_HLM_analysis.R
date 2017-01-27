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

# Filter for only the first 120 trials/subject to prevent fatigue confound
# SubTrial includes an extra few values from the intermission (SubTrial 61)
# So we filter for SubTrial <= 121
# dat <- dat %>% 
#   filter(SubTrial <= 121)

# Make correct-only dataset for RT analyses
dat.rt = dat %>% 
  filter(Probe.ACC == 1)

# Make fast-only dataset for ACC analyses
dat.acc = dat %>% 
  filter(feedbackmask == "fast")

# Accuracy analyses ----
# Does accuracy depend on 3-way interaction?
mAcc = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
               (1 + CueClass * Probe|Subject) + (1|ProbeType) + (1|CueType),
             data = dat.acc, family = "binomial",
             contrasts = list(Condition = contr.sum,
                              CueClass = contr.sum,
                              Probe = contr.sum))
summary(mAcc) 
ranef(mAcc)
# No, it does not. But note that this fails to converge max|grad| = .0018 vs tol = .001

# does lsmeans() work on glmer objects? Oh hell yeah it does
lsmeans(mAcc, c("Condition", "CueClass", "Probe")) %>% 
  summary() %>% 
  ggplot(aes(x = interaction(CueClass, Probe), y = lsmean, 
             ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_pointrange() +
  facet_wrap(~Condition) +
  geom_hline(yintercept = 1, lty = 2)

# Check it without random item terms 
mAcc.1 = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
               (1 + CueClass + Probe|Subject),
             data = dat.acc, family = "binomial",
             contrasts = list(Condition = contr.sum,
                              CueClass = contr.sum,
                              Probe = contr.sum))
# "model failed to converge, degenerate Hessian"
summary(mAcc.1) 
ranef(mAcc.1)

# 2x2 ANOVAs ----
# Model with random interactions would not converge, negative eigenvalues
# Random effect of CueClass perfectly correlated w/ Cue×Probe interaction
# Therefore this only models additive random slopes
# Same significant interaction with or without random intercepts of stimulus.
mAcc.GunNeu = dat.acc %>% 
  filter(Condition == "GunNeu") %>% 
  glmer(Probe.ACC ~ CueClass * Probe + 
          (1 + CueClass + Probe|Subject) + 
          (1|ProbeType) + (1|CueType), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = "contr.sum",
                         CueClass = "contr.sum"))
summary(mAcc.GunNeu) # Highly significant, p < .001
Anova(mAcc.GunNeu, type = 3)

# Very high correlation btwn interaction and main effect again worrisome, omitted
# This models only additive random slopes
# Again, results are the same whether including random item terms or not
mAcc.GunTool = dat.acc %>% 
  filter(Condition == "GunTool") %>% 
  glmer(Probe.ACC ~ CueClass * Probe + 
          (1 + CueClass + Probe|Subject) + 
          (1|ProbeType) + (1|CueType), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(mAcc.GunTool) # p = .001, although |max|grad| a bit above threshold (.0013)
# Note problematic correlation between Cue×Probe slope and Probe slope

# Pairwise contrasts for white stimuli ----
# Typical WIT
# Without random subject-slopes and item-intercepts
GunTool.white.acc = dat.acc %>% 
  filter(Condition == "GunTool", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1|Subject), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(GunTool.white.acc) # ns, p = .138

# with random everything
GunTool.white.acc.ranef = dat.acc %>% 
  filter(Condition == "GunTool", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1 + Probe|Subject) + (1|ProbeType) + (1|CueType), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(GunTool.white.acc.ranef) # ns, p = .634
rfx.guntool = ranef(GunTool.white.acc.ranef) # save to object
# inspect distribution of random intercepts of probeType
rfx.guntool$ProbeType %>% 
  mutate(., Item = row.names(.),
         Group = rep(c("Misc", "Gun"), each = 4)) %>%
  ggplot(aes(x = Item, y = `(Intercept)`, fill = Group)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))

# Gun - NotGun condition
# Here I run the same analysis several times with different random terms
# Without random subject-slopes and item-intercepts
GunNeu.white.acc = dat.acc %>% 
  filter(Condition == "GunNeu", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1|Subject), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(GunNeu.white.acc) # Highly significant, p = .001

# With random subject-slopes but no item-intercepts
GunNeu.white.acc.r = dat.acc %>% 
  filter(Condition == "GunNeu", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1+ Probe|Subject), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(GunNeu.white.acc.r) # p = .046

# With random subject-slopes and item-intercepts
# Once you add random intercepts of stimulus though it dissolves. Why's that?
GunNeu.white.acc.ranef = dat.acc %>% 
  filter(Condition == "GunNeu", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1+ Probe|Subject) + (1|ProbeType) + (1|CueType), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(GunNeu.white.acc.ranef) # p = .354
# Let's pick apart the random effects
rfx = ranef(GunNeu.white.acc.ranef) # save to object
# inspect distribution of Probe-effect across subjects
probe.fx = rfx$Subject$ProbeWeapon + fixef(GunNeu.white.acc.ranef)["Probe1"]
hist(probe.fx) 
t.test(probe.fx) # well, this is significant, so why isn't the summary() output?
# inspect distribution of random intercepts of probeType
rfx$ProbeType %>% 
  mutate(., Item = row.names(.),
         Group = rep(c("Misc", "Gun"), each = 4)) %>%
  ggplot(aes(x = Item, y = `(Intercept)`, fill = Group)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))
# The grip-strength tester is very different!

# Pairwise contrast, white primes, classic gun/tool WIT
# Without random subject-slopes and item intercepts
GunTool.white.acc = dat.acc %>% 
  filter(Condition == "GunTool", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1|Subject), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(GunTool.white.acc) # not significant, p = .138

# with random subject-slopes
GunTool.white.acc.r = dat.acc %>% 
  filter(Condition == "GunTool", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1 + Probe|Subject), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = "contr.sum"))
summary(GunTool.white.acc.r) # not significant, p = .371

# With random subject-slopes and item intercepts
GunTool.white.acc.ranef = dat.acc %>% 
  filter(Condition == "GunTool", CueClass == "white") %>% 
  glmer(Probe.ACC ~ Probe + (1 + Probe|Subject) + (1|ProbeType) + (1|CueType), 
        data = .,
        family = "binomial",
        contrasts = list(Probe = contr.sum))
summary(GunTool.white.acc.ranef) # Not significant, p = .634
ranef(GunTool.white.acc.ranef) # Why are random effects of CueType all exactly zero?
# Debug attempt
dat.acc %>% 
  filter(Condition == "GunTool", CueClass == "white") %>% 
  select(Probe, ProbeType, CueType) %>% 
  distinct() %>% 
  arrange(ProbeType, CueType) # It looks like it's all there. Hmmm.
# Plot these random effects too
rfx2 = ranef(GunTool.white.acc.ranef)
rfx2$ProbeType %>% 
  mutate(., Item = row.names(.),
         Group = rep(c("Tool", "Gun"), each = 4)) %>%
  ggplot(aes(x = Item, y = `(Intercept)`, fill = Group)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))

# Bayes -- how much evidence do I have against 3-way interaction?
# Well, BayesFactor doesn't do binomial outcomes yet, does it now?

# RT analyses ----
# Worried about these extremely high correlations among effects
mRT = lmer(Probe.RT ~ Condition * CueClass * Probe + 
             (1 + CueClass * Probe|Subject),
            data = dat.rt,
           contrasts = list(Condition = contr.sum,
                            CueClass = contr.sum,
                            Probe = contr.sum))
summary(mRT)
Anova(mRT, type = 3) # significant 2x2 but not 2x2x2, p < .001

# Singular RFX matrix if including interaction
mRT2 = lmer(log(Probe.RT) ~ Condition * CueClass * Probe + 
              (1 + CueClass + Probe|Subject),
            data = dat.rt,
            contrasts = list(Condition = contr.sum,
                             CueClass = contr.sum,
                             Probe = contr.sum))
summary(mRT2)
Anova(mRT2, type = 3) # sign. 2x2 but not 2x2x2

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

# Table export ----
# Aggregation and table export
# Should I use conditional trials (dat.acc, dat.rt) or unconditional (dat)?
# I'll use conditional b/c that's what's actually analyzed.
# Another question is whether to first average w/in subjects 
# or just average across all trials directly.
# I think it's better to average w/in subjects first.
S4T1.acc = dat.acc %>% 
  group_by(Condition, TrialType, Subject) %>% 
  summarize(M.sub = mean(Probe.ACC, na.rm = T))%>% 
  summarize(M.acc = mean(M.sub, na.rm = T),
            SD.acc = sd(M.sub, na.rm = T))

S4T1.rt = dat.rt %>% 
  group_by(Condition, TrialType, Subject) %>% 
  summarize(M.sub = mean(Probe.RT, na.rm = T))%>% 
  summarize(M.rt = mean(M.sub, na.rm = T),
            SD.rt = sd(M.sub, na.rm = T))

S4T1 = full_join(S4T1.acc, S4T1.rt)
write.table(S4T1, "S1table.txt", sep = "\t", row.names = F)

# This was stupid ----
# inspect distribution of random intercepts
rfx$Subject %>% 
  mutate(., Subject = row.names(.)) %>%
  arrange(`(Intercept)`) %>% 
  mutate(index = 1:nrow(.)) %>% 
  ggplot(aes(x = index, y = `(Intercept)`)) +
  geom_bar(stat = "identity")
# inspect distribution of random slopes of probe
rfx$Subject %>% 
  mutate(., Subject = row.names(.)) %>%
  arrange(ProbeWeapon) %>% 
  mutate(index = 1:nrow(.)) %>% 
  ggplot(aes(x = index, y = ProbeWeapon)) +
  geom_bar(stat = "identity")