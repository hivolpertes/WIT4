library(plyr)
library(dplyr)
library(ggplot2)

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

# 2-ways within each condition
mod2 <- dat.acc %>% 
  filter(Condition == "WhiteHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
            data = .)
summary(mod2)

mod3 <- dat.acc %>% 
  filter(Condition == "neutHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
      data = .)
summary(mod3)

mod4 <- dat.acc %>% 
  filter(Condition == "BlackHisp") %>% 
  aov(Probe.ACC ~ Prime * Target + Error(Subject/(Prime * Target)),
      data = .)
summary(mod4)

# Hispanic-prime trials only
mod5 <- dat.acc %>% 
  filter(Prime == "Hisp") %>% 
  aov(Probe.ACC ~ Condition * Target + Error(Subject/Target),
      data = .)
summary(mod5)