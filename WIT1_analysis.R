library(plyr)
library(dplyr)
library(MBESS)

# Function for making ESCI on partial eta-squared
# I'll stick with eta-sq b/c in Lakens (2013) says omega-sq
  # is too much a pain in the ass to calculate these days.
# Work in progress. Is this eta-sq or partial eta-sq?
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
# partial eta-sq
esci(SS = 1.84, SSE = .9428, conf = .90, df.1 = 2, df.2 = 78)

# 2-way interaction w/in each condition
mod2 <- dat.acc %>% 
  filter(Condition == "black_white") %>% 
  aov(Probe.ACC ~ Cue * Probe + Error(Subject/(Cue*Probe)),
      data = .)
summary(mod2)
esci(SS = .2441, SSE = .2842, conf = .90, df.1 = 1, df.2 = 25)

mod3 <- dat.acc %>% 
  filter(Condition == "neutral_black") %>% 
  aov(Probe.ACC ~ Cue * Probe + Error(Subject/(Cue*Probe)),
      data = .)
summary(mod3)
esci(SS = 1.1511, SSE = .3292, conf = .90, df.1 = 1, df.2 = 27)

mod4 <- dat.acc %>% 
  filter(Condition == "neutral_white") %>% 
  aov(Probe.ACC ~ Cue * Probe + Error(Subject/(Cue*Probe)),
      data = .)
summary(mod4)
esci(SS = .4500, SSE = .3294, conf = .90, df.1 = 1, df.2 = 26)

# 2-way interaction within each prime class
mod5 <- dat.acc %>% 
  filter(Cue == "Black") %>% 
  aov(Probe.ACC ~ Condition * Probe + Error(Subject/Probe), data = .)
summary(mod5)
esci(SS = .0025, SSE = .5592, conf = .90, df.1 = 1, df.2 = 52)

mod6 <- dat.acc %>% 
  filter(Cue == "Neutral") %>% 
  aov(Probe.ACC ~ Condition * Probe + Error(Subject/Probe), data = .)
summary(mod6)
esci(SS = .1245, SSE = .7549, conf = .90, df.1 = 1, df.2 = 53)


mod7 <- dat.acc %>% 
  filter(Cue == "White") %>% 
  aov(Probe.ACC ~ Condition * Probe + Error(Subject/Probe), data = .)
summary(mod7)
esci(SS = .2657, SSE = .5472, conf = .90, df.1 = 1, df.2 = 51)
