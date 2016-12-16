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

d.within <- function(x, y) {
  diff <- x-y
  mz <- mean(diff); sdz <- sd(diff)
  dz <- mz/sdz
  return(list(t.test(x, y, paired = T), dz))
}

dat.acc <- read.delim("acc_wit2.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject)) %>% 
  tbl_df()

dat.rt <- read.delim("rt_wit2.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject)) %>% 
  tbl_df()

# 3-way interaction
# Don't do this -- the model is singular
# mod1 <- aov(Probe.ACC ~ Condition * Prime * Target + Error(Subject/(Prime * Target)),
#             data = dat.acc)
# summary(mod1)
# esci(.4573, .3425, 3, 69, .90)

# 2-ways within each condition
mod4 <- dat.acc %>% 
  filter(Condition == "BlackHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
      data = .)
summary(mod4)
esci(.06217, .30090, 1, 19, .90)
esci(.05734, .04038, 1, 19, .90)

dat.acc %>% 
  filter(Condition == "BlackHisp", Prime == "Black") %>% 
  spread(Target, Probe.ACC) %>% 
  with(., d.within(Gun, Tool))

dat.acc %>% 
  filter(Condition == "BlackHisp", Prime == "Hisp") %>% 
  spread(Target, Probe.ACC) %>% 
  with(., d.within(Gun, Tool))


mod2 <- dat.acc %>% 
  filter(Condition == "WhiteHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
            data = .)
summary(mod2)
esci(.1611, .3434, 1, 26, .90)
esci(.00199, .08581, 1, 26, .90)

dat.acc %>% 
  filter(Condition == "WhiteHisp", Prime == "White") %>% 
  spread(Target, Probe.ACC) %>% 
  with(., d.within(Gun, Tool))

dat.acc %>% 
  filter(Condition == "WhiteHisp", Prime == "Hisp") %>% 
  spread(Target, Probe.ACC) %>% 
  with(., d.within(Gun, Tool))

mod3 <- dat.acc %>% 
  filter(Condition == "neutHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
      data = .)
summary(mod3)
esci(.3980, .2163, 1, 24, .90)

dat.acc %>% 
  filter(Condition == "neutHisp", Prime == "Neut") %>% 
  spread(Target, Probe.ACC) %>% 
  with(., d.within(Gun, Tool))

dat.acc %>% 
  filter(Condition == "neutHisp", Prime == "Hisp") %>% 
  spread(Target, Probe.ACC) %>% 
  with(., d.within(Gun, Tool))

# Hispanic-prime trials only
mod5 <- dat.acc %>% 
  filter(Prime == "Hisp") %>% 
  aov(Probe.ACC ~ Condition * Target + Error(Subject/Target),
      data = .)
summary(mod5)
esci(.2396, .6360, 1, 69, .90)
esci(.1066, .6360, 1, 69, .90)

# Means
dat.acc %>% 
  group_by(Condition, Prime, Target) %>% 
  summarise(mean_acc = mean(Probe.ACC))

# How do I do these follow-up contrasts?
# Liz Page-Gould has a thing at http://www.page-gould.com/r/anova/

# afex seems promising?
library(afex)
a1 <- aov_ez(id = "Subject", 
             dv = "Probe.ACC",
             data = filter(dat.acc, Prime == "Hisp"),
             between = "Condition",
             within = "Target")
nice(a1)

# All pairwise cells
m3 <- lsmeans(a1, ~ Target:Condition)
m3
pairs(m3)

# Custom contrasts
c1 <- list(
  bh_nh = c(.5, -.5, -.5, .5, 0, 0),
  bh_wh = c(.5, -.5, 0, 0, -.5, .5),
  nh_wh = c(0, 0, .5, -.5, -.5, .5))

contrast(m3, c1)
