library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(BayesFactor)
library(yarrr)
library(heplots)

dat.acc <- read.delim("acc_wit4.txt", stringsAsFactors = F) %>% 
  mutate(TargetGrp = ifelse(Target == "Gun", "Gun", "NotGun")) %>% 
  mutate_each(funs(as.factor), Subject, Condition, Prime, TargetGrp)
dat.rt <- read.delim("rt_wit4.txt", stringsAsFactors = F) %>% 
  mutate(TargetGrp = ifelse(Target == "Gun", "Gun", "NotGun")) %>% 
  mutate(Subject = factor(Subject))

# First, look for 2-way and/or 3-way interaction
mod1 <- aov(Probe.ACC ~ Condition * Prime * TargetGrp + Error(Subject/(Prime*TargetGrp)),
            data = dat.acc)
summary(mod1)

ggplot(dat.acc, aes(x = interaction(Prime,TargetGrp), y = Probe.ACC)) +
  geom_point() +
  geom_violin() +
  geom_boxplot(notch = T, width = .4) +
  facet_wrap(~Condition)

# Within GunTool WIT
mod2 <- dat.acc %>% 
  filter(Condition == "GunTool") %>% 
  aov(Probe.ACC ~ Prime * TargetGrp + Error(Subject/(Prime*TargetGrp)),
            data = .)
summary(mod2)

# Within GunNeu WIT
mod3 <- dat.acc %>% 
  filter(Condition == "GunNeu") %>% 
  aov(Probe.ACC ~ Prime * TargetGrp + Error(Subject/(Prime*TargetGrp)),
      data = .)
summary(mod3)

# Contrasts within White primes are not stat sig
mod4 <- dat.acc %>% 
  filter(Prime == "White" & Condition == "GunTool") %>% 
  aov(Probe.ACC ~ TargetGrp + Error(Subject/TargetGrp), data = .)
summary(mod4)

mod5 <- dat.acc %>% 
  filter(Prime == "White" & Condition == "GunNeu") %>% 
  aov(Probe.ACC ~ TargetGrp + Error(Subject/TargetGrp), data = .)
summary(mod5)

  # But of course, one can always go digging
mod6 <- dat.acc %>% 
  filter(Prime == "White") %>% 
  aov(Probe.ACC ~ Condition*TargetGrp + Error(Subject/TargetGrp), data = .)
summary(mod6)

# Can I even do Bayesian analyses? The within-subjects stuff seems to make it 
#  extremely computatationally expensive.
# Bayesian analysis
b1 <- anovaBF(Probe.ACC ~ Condition * Prime * TargetGrp + Subject, 
              data = dat.acc,
              whichRandom = "Subject")
b1 # decent support for Prime x Target interaction
