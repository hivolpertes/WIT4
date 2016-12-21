library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(BayesFactor)
library(MBESS)
library(yarrr)
library(heplots)

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
esci(.00004, .4370, 1, 69, .9) # Nothing to the 3-way
esci(.1243, .4370, 1, 69, .9) # Clearly something to the 2-way

# F = 0.00 is odd, let's double-check

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
esci(.0585, .2761, 1, 32, .9)

# Within GunNeu WIT
mod3 <- dat.acc %>% 
  filter(Condition == "GunNeu") %>% 
  aov(Probe.ACC ~ Prime * TargetGrp + Error(Subject/(Prime*TargetGrp)),
      data = .)
summary(mod3)
esci(.06585, .16096, 1, 37, .9)

# Contrasts within White primes are not stat sig
mod4 <- dat.acc %>% 
  filter(Prime == "White" & Condition == "GunTool") %>% 
  aov(Probe.ACC ~ TargetGrp + Error(Subject/TargetGrp), data = .)
summary(mod4)
esci(.0188, .3415, 1, 32, .9)

# A t-test could be a nicer parameterization

mod5 <- dat.acc %>% 
  filter(Prime == "White" & Condition == "GunNeu") %>% 
  aov(Probe.ACC ~ TargetGrp + Error(Subject/TargetGrp), data = .)
summary(mod5)
esci(.03104, .30143, 1, 37, .9)

  # But of course, one can always go digging
mod6 <- dat.acc %>% 
  filter(Prime == "White") %>% 
  aov(Probe.ACC ~ Condition*TargetGrp + Error(Subject/TargetGrp), data = .)
summary(mod6)
esci(.0494, .6429, 1, 69, .9)

# Bayesian ----
# Bayesian analysis, all cells
b1 <- anovaBF(Probe.ACC ~ Condition * Prime * TargetGrp + Subject, 
              data = dat.acc,
              whichRandom = "Subject")
b1 # decent support for Prime x Target interaction w/o 3-way
b1["Prime + TargetGrp + Prime:TargetGrp + Subject"]/b1["Prime + TargetGrp + Subject"]
b1["Prime + TargetGrp + Prime:TargetGrp + Subject"]/b1["Prime + TargetGrp + Subject"]

# Bayesian analysis, within GunTool
b2 <- dat.acc %>% 
  filter(Condition == "GunTool") %>% 
  anovaBF(Probe.ACC ~ Prime * TargetGrp + Subject,
          whichRandom = "Subject",
          data = .)
b2 # Nothing???
b2[4]/b2[3] # 5.4:1 odds for the interaction

# Bayesian analysis, within GunNeu
b3 <- dat.acc %>% 
  filter(Condition == "GunNeu") %>% 
  anovaBF(Probe.ACC ~ Prime * TargetGrp + Subject,
          whichRandom = "Subject",
          data = .)
b3 # Nothing???
b3[4]/b3[3] # 17.7:1 odds for the interaction

# Is it a parameterization thing?
