library(plyr)
library(dplyr)

dat.acc <- read.delim("acc_wit1.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject),
         Condition = substring(ExperimentName, 5))
dat.rt <- read.delim("rt_wit1.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject),
         Condition = substring(ExperimentName, 5))

# 3-way interaction
mod1 <- aov(Probe.ACC ~ ExperimentName * Cue * Probe + Error(Subject/(Cue*Probe)),
            data = dat.acc)
summary(mod1)

# 2-way interaction w/in each condition
mod2 <- dat.acc %>% 
  filter(Condition == "black_white") %>% 
  aov(Probe.ACC ~ Cue * Probe + Error(Subject/(Cue*Probe)),
      data = .)
summary(mod2)

mod3 <- dat.acc %>% 
  filter(Condition == "neutral_black") %>% 
  aov(Probe.ACC ~ Cue * Probe + Error(Subject/(Cue*Probe)),
      data = .)
summary(mod3)

mod4 <- dat.acc %>% 
  filter(Condition == "neutral_white") %>% 
  aov(Probe.ACC ~ Cue * Probe + Error(Subject/(Cue*Probe)),
      data = .)
summary(mod4)

# 2-way interaction within each prime class
mod5 <- dat.acc %>% 
  filter(Cue == "Black") %>% 
  aov(Probe.ACC ~ Condition * Probe + Error(Subject/Probe), data = .)
summary(mod5)

mod6 <- dat.acc %>% 
  filter(Cue == "Neutral") %>% 
  aov(Probe.ACC ~ Condition * Probe + Error(Subject/Probe), data = .)
summary(mod6)

mod7 <- dat.acc %>% 
  filter(Cue == "White") %>% 
  aov(Probe.ACC ~ Condition * Probe + Error(Subject/Probe), data = .)
summary(mod7)