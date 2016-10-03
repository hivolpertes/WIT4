library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(car)

dat.acc <- read.delim("acc_wit3.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject),
         Probe = ifelse(ProbeClass == "WEAP", "Gun", "Not-Gun"))
dat.rt <- read.delim("rt_wit3.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject),
         Probe = ifelse(ProbeClass == "WEAP", "Gun", "Not-Gun"))

# 3-way interaction?
mod1 <- aov(Probe.ACC ~ CueClass * Probe * Condition + Error(Subject/(CueClass*Probe)), 
            data = dat.acc)
summary(mod1)

# Standard WIT effect?
mod2 <- dat.acc %>% 
  filter(Condition == "GunTool") %>% 
  aov(Probe.ACC ~ CueClass * Probe + Error(Subject/(CueClass*Probe)),
            data = .)
summary(mod2)

# and in Black/Gun task?
mod3 <- dat.acc %>% 
  filter(Condition == "BlackGun") %>% 
  aov(Probe.ACC ~ CueClass * Probe + Error(Subject/(CueClass*Probe)),
      data = .)
summary(mod3)

# Specific test on Gun-target trials
mod4 <- dat.acc %>% 
  filter(Probe == "Gun") %>% 
  aov(Probe.ACC ~ CueClass * Condition + Error(Subject/CueClass),
      data = .)
summary(mod4)