library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(compute.es)

dat <- read.delim(file="clean_wit1.txt")

# let's define Automatic in terms of bias towards gun
# e.g. A=1 means always automatic priming of gun by prime
# whereas A=0 means always automatic priming of tool by prime
# I did the algebra and you get the same C parameter and A1 = 1-A2 so its all good

# Thus, we're not conditioning on experiment, just 

# calculate A for White, Black, Object primes w/in each condition
sumStats <- dat %>% 
  group_by(Subject, ExperimentName, Cue, Probe) %>% 
  summarise(Accuracy = mean(Probe.ACC)) %>% 
  ungroup()

# from Stewart & Payne, 2008, p1338:
# C = P(correct | congruent) - P(stereotypic error | incongruent)
# A = P(stereotypic error | incongruent) / (1-C)

# C is constant given A or (1-A)
# So flipping A to be 1-A does not change C.

# I will calculate
# C1 = P(Correct|Gun, Cue1) - P(Incorrect|Tool, Cue1)
# C2 = P(Correct|Gun, Cue2) - P(Incorrect|Tool, Cue2)
# A1 = P(Gun-response|Tool, Cue1) / (1 - C1)
# A2 = P(Gun-response|Tool, Cue2) / (1 - C2)

# Calculate "Control"
control <- sumStats %>% 
  # Average across cues
  #group_by(ExperimentName, Subject, Probe) %>% 
  #summarize(Accuracy = mean(Accuracy)) %>%  
  # Make difference of Gun-Accuracy and Tool-Error
  spread(key = Probe, value = Accuracy) %>% 
  mutate(C = Gun - (1 - Tool)) %>% 
  select(-Gun, -Tool)

# Bind to original sumStats frame
sumStats2 <- full_join(sumStats, control)

# Calculate "Auto"
auto <- sumStats2 %>% 
  spread(key = Probe, value = Accuracy) %>% 
  # Auto = (Tool-trial errors) / (1 - C)
  mutate(A = (1 - Tool)/(1 - C)) %>% 
  select(-Gun, -Tool)

# Bind to data frame
datPDP <- full_join(sumStats2, auto) %>% 
  # Make prettier column names
  mutate(Condition = mapvalues(ExperimentName, 
                               c("WIT_black_white", "WIT_neutral_black", "WIT_neutral_white"),
                               c("BW", "BN", "WN"))) %>% 
  # Drop redundant columns / rows
  select(-ExperimentName, -Accuracy, -Probe) %>% 
  distinct()

# save
write.table(datPDP, file="WIT_study1_PDP.txt", sep="\t", row.names=F)


# the general model fails to converge probably due to
# insufficient degrees of freedom / missing cells
# it's a pain but I'll need to run more specific contrasts...

BlackA = lm(A ~ Condition, data = datPDP, subset = Cue == "Black")
BlackC = lm(C ~ Condition, data = datPDP, subset = Cue == "Black")
summary(BlackA)
summary(BlackC)

WhiteA = lm(A ~ Condition, data = datPDP, subset = Cue == "White")
WhiteC = lm(C ~ Condition, data = datPDP, subset = Cue == "White")
summary(WhiteA)
summary(WhiteC)

NeutralA = lm(A ~ Condition, dat=datPDP, subset = Cue == "Neutral")
NeutralC = lm(C ~ Condition, dat=datPDP, subset = Cue == "Neutral")
summary(NeutralA)
summary(NeutralC)

# graphics ----
# these histograms would seem to indicate that
# C is holding mostly constant while
# A is shifting fairly dramatically
# Plots of C
ggplot(datPDP, aes(x=C)) +
  geom_histogram(binwidth=.1) +
  facet_grid(Condition~Cue)

ggplot(datPDP, aes(x = Condition, y = C)) +
  geom_violin() +
  geom_point() +
  facet_grid(Cue~.) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))

ggplot(datPDP, aes(x = Condition, y = C)) +
  geom_violin() +
  geom_boxplot(width = .5, notch = T) +
  facet_grid(Cue~.) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))

# Plots of A
ggplot(datPDP, aes(x=A)) +
  geom_histogram(binwidth=.1) +
  facet_grid(Condition~Cue)

ggplot(datPDP, aes(x = Condition, y = A)) +
  geom_violin() +
  geom_point() +
  facet_wrap(~Cue) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))

ggplot(datPDP, aes(x = Condition, y = A)) +
  geom_violin() +
  geom_boxplot(width = .35, notch = T) +
  facet_wrap(~Cue) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))