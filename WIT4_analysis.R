library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)

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
         CueClass = substr(CueType, 1, 5)
  )

# Make correct-only dataset for RT analyses
dat.rt = dat %>% 
  filter(Probe.ACC == 1)

# Does accuracy depend on Condition x Prime interaction?
#mAcc = lmer(Probe.ACC ~ Condition * + (1|Subject))
