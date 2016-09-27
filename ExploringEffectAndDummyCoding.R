# What is the contrasts argument in glmer and lmer doing? 
# Couldn't figure out what the hell was going on
# But you can manually recreate the same models by creating effect codes (and keeping them as numeric variables,
# can't make them factors)

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

dat.rt.GN = filter(dat.rt, Condition == "GunNeu")
dat.rt.GN$ProbeClass = factor(dat.rt.GN$ProbeClass)

#################################
dat.rt.GN %>% 
  group_by(CueClass, ProbeClass) %>% 
  summarise(avg = mean(Probe.RT))

# 1    Black    Neutral 426.2100
# 2    Black     Weapon 382.0162
# 3    white    Neutral 414.7602
# 4    white     Weapon 384.1441

dat.rt.GN %>% 
  group_by(CueClass) %>% 
  summarise(avg = mean(Probe.RT))

# 1    Black 403.8323
# 2    white 399.9824

dat.rt.GN %>% 
  group_by(ProbeClass) %>% 
  summarise(avg = mean(Probe.RT))

# 1    Neutral 420.3664
# 2     Weapon 383.0518

#################################

# default R coding when contrast isn't specified
lmer(Probe.RT ~ CueClass * ProbeClass + (1 + ProbeClass|Subject) + (1|ProbeType), 
     data = dat.rt.GN) %>% 
  summary()

# equivalent to dummy coding, such that 0 and 1 are assigned in alphabetical order
dat.rt.GN$CueDummy[dat.rt.GN$CueClass == "white"] = 1     # can switch this to look at weapon effect for white trials
dat.rt.GN$CueDummy[dat.rt.GN$CueClass == "Black"] = 0

dat.rt.GN$ProbeDummy[dat.rt.GN$ProbeClass == "Neutral"] = 0
dat.rt.GN$ProbeDummy[dat.rt.GN$ProbeClass == "Weapon"] = 1

lmer(Probe.RT ~ CueDummy * ProbeDummy + (1 + ProbeDummy|Subject) + (1|ProbeType), 
     data = dat.rt.GN) %>% 
  summary()

# betas for one fixed variable are interpreted at level 0 of other categorical variable

############################

# What about effect coding?

lmer(Probe.RT ~ CueClass * ProbeClass + (1 + ProbeClass|Subject) + (1|ProbeType), 
     data = dat.rt.GN, contrasts = list(CueClass = contr.sum,
                                        ProbeClass = contr.sum)) %>% 
  summary()

# recreate by manually effect coding
dat.rt.GN$CueEffect[dat.rt.GN$CueClass == "white"] = -1
dat.rt.GN$CueEffect[dat.rt.GN$CueClass == "Black"] = 1

dat.rt.GN$ProbeEffect[dat.rt.GN$ProbeClass == "Neutral"] = -1
dat.rt.GN$ProbeEffect[dat.rt.GN$ProbeClass == "Weapon"] = 1

# effect coding
lmer(Probe.RT ~ CueEffect * ProbeEffect + (1 + ProbeEffect|Subject) + (1|ProbeType), 
     data = dat.rt.GN) %>% 
  summary()

# scale of effect codes change the scale of the betas, but not the test statistics
# huh, I didn't know that
