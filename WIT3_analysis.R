library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(afex)
library(MBESS)

esci <- function(SS, SSE, df.1, df.2, conf.level, digits = 2) {
  partial_eta <- SS/(SS + SSE)
  Lims <- conf.limits.ncf(F.value = (SS/df.1)/(SSE/df.2), 
                          df.1 <- df.1, df.2 <- df.2,
                          conf.level = conf.level)
  Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
  if(is.na(Lower.lim)) Lower.lim <- 0
  Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
  out <- list("etasq" = partial_eta, "CI" = c(Lower.lim, Upper.lim))
  out <- lapply(out, round, digits = digits)
  return(out)
}

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
esci(.0525, .3390, 1, 54, .9)

# Standard WIT effect?
mod2 <- dat.acc %>% 
  filter(Condition == "GunTool") %>% 
  aov(Probe.ACC ~ CueClass * Probe + Error(Subject/(CueClass*Probe)),
            data = .)
summary(mod2)
esci(.0360, .2507, 1, 31, .9)

# and in Black/Gun task?
mod3 <- dat.acc %>% 
  filter(Condition == "BlackGun") %>% 
  aov(Probe.ACC ~ CueClass * Probe + Error(Subject/(CueClass*Probe)),
      data = .)
summary(mod3)
esci(.01927, .08835, 1, 23, .9)

# Specific test on Gun-target trials
mod4 <- dat.acc %>% 
  filter(Probe == "Gun") %>% 
  aov(Probe.ACC ~ CueClass * Condition + Error(Subject/CueClass),
      data = .)
summary(mod4)
esci(.0187, .3383, 1, 54, .9)

# Is it powered by the not-gun-target trials, then?
mod5 <- dat.acc %>% 
  filter(Probe == "Not-Gun") %>% 
  aov(Probe.ACC ~ CueClass * Condition + Error(Subject/CueClass),
      data = .)
summary(mod5)
esci(.0351, .3419, 1, 54, .9)

# Table
wit3means <- dat.acc %>% 
  group_by(Condition, CueClass, Probe) %>% 
  summarize(acc = round(mean(Probe.ACC), 3))

# interpreting the 3-way
filter(wit3means, Probe == "Gun")
# Black primes increase gun-trial accuracy in Gun/Tool task, 
# but decrease accuracy for Gun/Black task
# But not significant, p = .09
filter(wit3means, Probe == "Not-Gun")
# Black primes increase black-target accuracy in Black/Gun task,
# Black primes decrease tool-target accuracy in Black/Tool task

ggplot(wit3means, aes(x = Probe, y = acc)) +
  geom_bar(stat = "identity") +
  facet_grid(Condition ~ CueClass)
