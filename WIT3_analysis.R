library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(afex)
library(MBESS)
library(BayesFactor)

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
         Probe = ifelse(ProbeClass == "WEAP", "Gun", "Not-Gun")) %>% 
  mutate(CueClass = as.factor(CueClass),
         Probe = as.factor(Probe),
         Condition = as.factor(Condition))
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

# Bayes ----
# Keep in mind this "linear probability model" is not ideal model
# 3-way interaction?
mod1BF <- anovaBF(Probe.ACC ~ CueClass * Probe * Condition + Subject,
                whichRandom = "Subject", 
                data = dat.acc,
                iterations = 5e4)
summary(mod1BF)
sort(mod1BF)

# Best model: Condition + Probe + Condition:Probe
mod1BF[10]
sort(mod1BF)[18]

# Compare 3-way interaction to 2-ways only model
mod1BF[17]/mod1BF[18] # Ambiguous
# Compare 3-way interaction to ...?


# Standard WIT effect?
mod2BF <- dat.acc %>% 
  filter(Condition == "GunTool") %>% 
  anovaBF(Probe.ACC ~ CueClass * Probe + Subject,
          whichRandom = "Subject",
          data = .,
          iterations = 5e4)
summary(mod2BF)

# Mild evidence against the usual 2-way interaction,
# Best model is just main effect of probe


# and in Black/Gun task?
mod3BF <- dat.acc %>% 
  filter(Condition == "BlackGun") %>% 
  anovaBF(Probe.ACC ~ CueClass * Probe + Subject,
          whichRandom = "Subject",
          data = .)
summary(mod3BF)
# Again, best model is just a main effect of Probe

# Specific test on Gun-target trials
mod4BF <- dat.acc %>% 
  filter(Probe == "Gun") %>% 
  anovaBF(Probe.ACC ~ CueClass * Condition + Subject,
      whichRandom = "Subject",
      data = .)
summary(mod4BF)
# Nothing! But the evidence regarding the interaction is only ambiguous
mod4BF[4]/mod4BF[3]

