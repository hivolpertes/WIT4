library(plyr)
library(dplyr)
library(ggplot2)
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

dat.acc <- read.delim("acc_wit2.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject)) %>% 
  tbl_df()

dat.rt <- read.delim("rt_wit2.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject)) %>% 
  tbl_df()

# 3-way interaction
mod1 <- aov(Probe.ACC ~ Condition * Prime * Target + Error(Subject/(Prime * Target)),
            data = dat.acc)
summary(mod1)
esci(.4573, .3425, 3, 69, .90)

# 2-ways within each condition
mod2 <- dat.acc %>% 
  filter(Condition == "WhiteHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
            data = .)
summary(mod2)
esci(.00199, .08581, 1, 26, .90)

mod3 <- dat.acc %>% 
  filter(Condition == "neutHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
      data = .)
summary(mod3)
esci(.3980, .2163, 1, 24, .90)

mod4 <- dat.acc %>% 
  filter(Condition == "BlackHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
      data = .)
summary(mod4)
esci(.05734, .04038, 1, 19, .90)

# Hispanic-prime trials only
mod5 <- dat.acc %>% 
  filter(Prime == "Hisp") %>% 
  aov(Probe.ACC ~ Condition * Target + Error(Subject/Target),
      data = .)
summary(mod5)
esci(.2396, .6360, 1, 69, .90)
