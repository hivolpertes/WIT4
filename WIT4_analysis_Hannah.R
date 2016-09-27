require(lme4)
require(lmerTest)
require(plyr)
require(dplyr)

# Model specification procedures for accuracy data and RT data (separately for each group of subjects that did each 
# task [between subjects variable] and then with both task data together)

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

# Make correct-only dataset for RT analyses
dat.rt = dat %>% 
  filter(Probe.ACC == 1)

# Make fast-only dataset for ACC analyses
dat.acc = dat %>% 
  filter(feedbackmask == "fast")


# ACCURACY ----------------------------------------------------------------


# First look at subjects who did GunTool task -----------------------------

dat.acc.GT = filter(dat.acc, Condition == "GunTool")

# test grouping variables

m1 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|ProbeType) + (1|CueType), data = dat.acc.GT, family = "binomial")

# test subject as grouping variable
m2 = glmer(Probe.ACC ~ 1 + (1|ProbeType) + (1|CueType), data = dat.acc.GT, family = "binomial")
anova(m1, m2)

# test probetype as grouping variable
m3 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|CueType), data = dat.acc.GT, family = "binomial")
anova(m1, m3)

# test cuetype as grouping variable
m4 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|ProbeType), data = dat.acc.GT, family = "binomial")
anova(m1, m4)
# LRT isn't significant, so cuetype doesn't add anything, better to simplify and take it out
# use subject and probetype as grouping variables

# figure out which random slopes to use
m5 = glmer(Probe.ACC ~ CueClass * Probe + 
               (1 + CueClass * Probe|Subject) + (1|ProbeType),
             data = dat.acc.GT, family = "binomial",
           contrasts = list(CueClass = contr.sum,
                            Probe = contr.sum))
summary(m5) 
# Fails to converge, need to reduce number of random effects

m6 = glmer(Probe.ACC ~ CueClass * Probe + 
             (1 + CueClass + Probe|Subject) + (1|ProbeType),
           data = dat.acc.GT, family = "binomial",
           contrasts = list(CueClass = contr.sum,
                            Probe = contr.sum))
summary(m6) 
# Converges, but correlation is still high [Check with Ed]. If need to take out another random slope, take out cueClass
# because more variance in slope of probe

m7 = glmer(Probe.ACC ~ CueClass * Probe + 
             (1 + Probe|Subject) + (1|ProbeType),
           data = dat.acc.GT, family = "binomial",
           contrasts = list(CueClass = contr.sum,
                            Probe = contr.sum))
summary(m7) 
#still somewhat high but we'll go with it

# Final model for GunTool task --------------------------------------------
m.acc.GT = glmer(Probe.ACC ~ CueClass * Probe + 
             (1+Probe|Subject) + (1|ProbeType),
           data = dat.acc.GT, family = "binomial",
           contrasts = list(CueClass = contr.sum, # effect coding for both categorical variables
                            Probe = contr.sum))
summary(m.acc.GT) # realized that intercept is above one- should represent grand mean of Y, so shouldn't it be less than 1?

# sink(file = "./WIT4/GunToolData_accuracy.txt")
# summary(m.acc.GT)
# "________________________________________________________________________________________________"
# coef(m.acc.GT)
# sink()



# Do the same for GunNeutral task ----------------------------------------

dat.acc.GN = filter(dat.acc, Condition == "GunNeu")

# test grouping variables

m1 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|ProbeType) + (1|CueType), data = dat.acc.GN, family = "binomial")

# test subject as grouping variable
m2 = glmer(Probe.ACC ~ 1 + (1|ProbeType) + (1|CueType), data = dat.acc.GN, family = "binomial")
anova(m1, m2)

# test probetype as grouping variable
m3 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|CueType), data = dat.acc.GN, family = "binomial")
anova(m1, m3)

# test cuetype as grouping variable
m4 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|ProbeType), data = dat.acc.GN, family = "binomial")
anova(m1, m4)
# LRT isn't significant, so cuetype doesn't add anything, better to simplify and take it out
# use subject and probetype as grouping variables

# figure out which random slopes to use
m5 = glmer(Probe.ACC ~ CueClass * Probe + 
             (1 + CueClass * Probe|Subject) + (1|ProbeType),
           data = dat.acc.GN, family = "binomial",
           contrasts = list(CueClass = contr.sum,
                            Probe = contr.sum))
summary(m5) 
# Converges, but correlations among random effects effects is high. 

m6 = glmer(Probe.ACC ~ CueClass * Probe + 
             (1 + CueClass + Probe|Subject) + (1|ProbeType),
           data = dat.acc.GN, family = "binomial",
           contrasts = list(CueClass = contr.sum,
                            Probe = contr.sum))
summary(m6) 
# correlation is still high

m7 = glmer(Probe.ACC ~ CueClass * Probe + 
             (1 + Probe|Subject) + (1|ProbeType),
           data = dat.acc.GN, family = "binomial",
           contrasts = list(CueClass = contr.sum,
                            Probe = contr.sum))
summary(m7) 
# looks good, we'll use this.

# Final model for GunNeutral task --------------------------------------------
m.acc.GN = glmer(Probe.ACC ~ CueClass * Probe + 
             (1 + Probe|Subject) + (1|ProbeType),
           data = dat.acc.GN, family = "binomial",
           contrasts = list(CueClass = contr.sum,
                            Probe = contr.sum))
summary(m.acc.GN) 

# sink(file = "./WIT4/GunNeuData_accuracy.txt")
# summary(m.acc.GN)
# "________________________________________________________________________________________________"
# coef(m.acc.GN)
# sink()


# Now look at all subjects together ---------------------------------------

# test grouping variables

m1 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|ProbeType) + (1|CueType), data = dat.acc, family = "binomial")

# test subject as grouping variable
m2 = glmer(Probe.ACC ~ 1 + (1|ProbeType) + (1|CueType), data = dat.acc, family = "binomial")
anova(m1, m2)

# test probetype as grouping variable
m3 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|CueType), data = dat.acc, family = "binomial")
anova(m1, m3)

# test cuetype as grouping variable
m4 = glmer(Probe.ACC ~ 1 + (1|Subject) + (1|ProbeType), data = dat.acc, family = "binomial")
anova(m1, m4)
# cue type is not significant, so don't use as grouping variable

# figure out which random slopes to use
m5 = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
             (1 + Condition * CueClass * Probe|Subject) + (1|ProbeType),
           data = dat.acc, family = "binomial",
           contrasts = list(Condition = contr.sum,
                            CueClass = contr.sum,
                            Probe = contr.sum))
summary(m5) 
# fails to converge

m6 = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
             (1 + Condition + CueClass * Probe|Subject) + (1|ProbeType),
           data = dat.acc, family = "binomial",
           contrasts = list(Condition = contr.sum, #coefficients for each categorical are constrained to add up to 0
                            CueClass = contr.sum,
                            Probe = contr.sum))
summary(m6) 
# failed to converge

m7 = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
             (1 + Condition + CueClass + Probe|Subject) + (1|ProbeType),
           data = dat.acc, family = "binomial",
           contrasts = list(Condition = contr.sum, #coefficients for each categorical are constrained to add up to 0
                            CueClass = contr.sum,
                            Probe = contr.sum))
summary(m7) 
# still failed to converge. Now have to start choosing which slopes to leave out. 
# cueClass has least amount of variance so take that out first

m8 = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
             (1 + Condition + Probe|Subject) + (1|ProbeType),
           data = dat.acc, family = "binomial",
           contrasts = list(Condition = contr.sum, #coefficients for each categorical are constrained to add up to 0
                            CueClass = contr.sum,
                            Probe = contr.sum))
summary(m8) 
# converges, but still a high correlation among random effects. More variance in 
# condition, so take out probe

m9 = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
             (1 + Condition|Subject) + (1|ProbeType),
           data = dat.acc, family = "binomial",
           contrasts = list(Condition = contr.sum, #coefficients for each categorical are constrained to add up to 0
                            CueClass = contr.sum,
                            Probe = contr.sum))
summary(m9) 
# slope of condition is still highly correlated with intercept, so take that out


# Final model for both tasks ----------------------------------------------

# basically best model is varying intercept model
m.acc.Both = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
             (1|Subject) + (1|ProbeType),
           data = dat.acc, family = "binomial",
           contrasts = list(Condition = contr.sum, #coefficients for each categorical are constrained to add up to 0
                            CueClass = contr.sum,
                            Probe = contr.sum))
summary(m.acc.Both) 

# sink(file = "./WIT4/allData_accuracy.txt")
# summary(m.acc.Both)
# "________________________________________________________________________________________________"
# coef(m.acc.Both)
# sink()



# REACTION TIME -----------------------------------------------------------


# First look at subjects who did GunTool task -----------------------------

dat.rt.GT = filter(dat.rt, Condition == "GunTool")

# test grouping variables

m1 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|ProbeType) + (1|CueType), data = dat.rt.GT)

# test subject as grouping variable
m2 = lmer(Probe.RT ~ 1 + (1|ProbeType) + (1|CueType), data = dat.rt.GT)
anova(m1, m2)

# test probetype as grouping variable
m3 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|CueType), data = dat.rt.GT)
anova(m1, m3)

# test cuetype as grouping variable
m4 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|ProbeType), data = dat.rt.GT)
anova(m1, m4)
# LRT isn't significant, so cuetype doesn't add anything, better to simplify and take it out
# use subject and probetype as grouping variables

# figure out which random slopes to use
m5 = lmer(Probe.RT ~ CueClass * ProbeClass + 
             (1 + CueClass * ProbeClass|Subject) + (1|ProbeType),
           data = dat.rt.GT, contrasts = list(CueClass = contr.sum,
                                              ProbeClass = contr.sum))
summary(m5) 
# correlations among random effects effects is high, means the data can't support that complex
# of a model. So, need to reduce number of random effects

m6 = lmer(Probe.RT ~ CueClass * ProbeClass + 
            (1 + CueClass + ProbeClass|Subject) + (1|ProbeType),
          data = dat.rt.GT, contrasts = list(CueClass = contr.sum,
                                             ProbeClass = contr.sum))
summary(m6) 
# correlation is still high so need to take something else out. More variance in slope
# of probe, so keep that random

m7 = lmer(Probe.RT ~ CueClass * ProbeClass + 
            (1 + ProbeClass|Subject) + (1|ProbeType),
          data = dat.rt.GT, contrasts = list(CueClass = contr.sum,
                                             ProbeClass = contr.sum))
summary(m7) 

# Final model for GunTool task --------------------------------------------
m.rt.GT = lmer(Probe.RT ~ CueClass * ProbeClass + 
            (1 + ProbeClass|Subject) + (1|ProbeType),
          data = dat.rt.GT, contrasts = list(CueClass = contr.sum,
                                             ProbeClass = contr.sum))
summary(m.rt.GT) 

# sink(file = "./WIT4/GunToolData_reactionTime.txt")
# summary(m.rt.GT)
# "________________________________________________________________________________________________"
# coef(m.rt.GT)
# sink()



# Now look at subjects who did GunNeu task -----------------------------

dat.rt.GN = filter(dat.rt, Condition == "GunNeu")

# test grouping variables

m1 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|ProbeType) + (1|CueType), data = dat.rt.GN)

# test subject as grouping variable
m2 = lmer(Probe.RT ~ 1 + (1|ProbeType) + (1|CueType), data = dat.rt.GN)
anova(m1, m2)

# test probetype as grouping variable
m3 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|CueType), data = dat.rt.GN)
anova(m1, m3)

# test cuetype as grouping variable
m4 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|ProbeType), data = dat.rt.GN)
anova(m1, m4)
# LRT isn't significant, so cuetype doesn't add anything, better to simplify and take it out
# use subject and probetype as grouping variables

# figure out which random slopes to use
m5 = lmer(Probe.RT ~ CueClass * ProbeClass + 
            (1 + CueClass * ProbeClass|Subject) + (1|ProbeType),
          data = dat.rt.GN, contrasts = list(CueClass = contr.sum,
                                             ProbeClass = contr.sum))
summary(m5) 
# doesn't converge

m6 = lmer(Probe.RT ~ CueClass * ProbeClass + 
            (1 + CueClass + ProbeClass|Subject) + (1|ProbeType),
          data = dat.rt.GN, contrasts = list(CueClass = contr.sum,
                                             ProbeClass = contr.sum))
summary(m6) 
# still doesn't converge

m7 = lmer(Probe.RT ~ CueClass * ProbeClass + 
            (1 + ProbeClass|Subject) + (1|ProbeType),
          data = dat.rt.GN, contrasts = list(CueClass = contr.sum,
                                             ProbeClass = contr.sum))
summary(m7) 

# Final model for GunNeu task --------------------------------------------
m.rt.GN = lmer(Probe.RT ~ CueClass * ProbeClass + 
                 (1 + ProbeClass|Subject) + (1|ProbeType),
               data = dat.rt.GN, contrasts = list(CueClass = contr.sum,
                                                  ProbeClass = contr.sum))
summary(m.rt.GN) 

# sink(file = "./WIT4/GunNeuData_reactionTime.txt")
# summary(m.rt.GN)
# "________________________________________________________________________________________________"
# coef(m.rt.GN)
# sink()



# Now look at all subjects together ---------------------------------------

# test grouping variables

m1 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|ProbeType) + (1|CueType), data = dat.rt)

# test subject as grouping variable
m2 = lmer(Probe.RT ~ 1 + (1|ProbeType) + (1|CueType), data = dat.rt)
anova(m1, m2)

# test probetype as grouping variable
m3 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|CueType), data = dat.rt)
anova(m1, m3)

# test cuetype as grouping variable
m4 = lmer(Probe.RT ~ 1 + (1|Subject) + (1|ProbeType), data = dat.rt)
anova(m1, m4)
# LRT isn't significant, so cuetype doesn't add anything, better to simplify and take it out
# use subject and probetype as grouping variables

# figure out which random slopes to use
m5 = lmer(Probe.RT ~ Condition * CueClass * Probe + 
            (1 + CueClass * Probe|Subject) + (1|ProbeType),
          data = dat.rt, contrasts = list(CueClass = contr.sum,
                                             Probe = contr.sum,
                                          Condition = contr.sum))
summary(m5) 
# doesn't converge

m6 = lmer(Probe.RT ~ Condition * CueClass * Probe + 
            (1 + CueClass + Probe|Subject) + (1|ProbeType),
          data = dat.rt, contrasts = list(CueClass = contr.sum,
                                             Probe = contr.sum,
                                          Condition = contr.sum))
summary(m6) 
# somewhat high correlations between random effects. Take out random slope
# of cue class

m7 = lmer(Probe.RT ~ Condition * CueClass * Probe + 
            (1 + Probe|Subject) + (1|ProbeType),
          data = dat.rt, contrasts = list(CueClass = contr.sum,
                                             Probe = contr.sum,
                                          Condition = contr.sum))
summary(m7) 

# Final model for both tasks --------------------------------------------
m.rt.Both = lmer(Probe.RT ~ Condition * CueClass * Probe + 
            (1 + Probe|Subject) + (1|ProbeType),
          data = dat.rt, contrasts = list(CueClass = contr.sum,
                                          Probe = contr.sum,
                                          Condition = contr.sum))
summary(m.rt.Both) 

# sink(file = "./WIT4/allData_reactionTime.txt")
# summary(m.rt.Both)
# "________________________________________________________________________________________________"
# coef(m.rt.Both)
# sink()


lmer(Probe.RT ~ Condition * CueClass * Probe + (1 + Probe|Subject) + (1|ProbeType), data = dat.rt) %>% summary()
