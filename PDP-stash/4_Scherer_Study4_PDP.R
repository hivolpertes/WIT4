library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(BayesFactor)
# How to get effect sizes?
library(compute.es)
# library(afex)
# library(ez)

dat <- read.delim(file="clean_wit4.txt") %>% 
  select(Subject, Condition, Probe.ACC, TrialType) %>% 
  separate(TrialType, into = c("Cue", "Probe1"), sep = 5) %>% 
  mutate(Probe = ifelse(Probe1 == "Gun", "Gun", "NotGun"))

# let's define Automatic in terms of bias towards gun
# e.g. A=1 means always automatic priming of gun by prime
# whereas A=0 means always automatic priming of tool by prime
# I did the algebra and you get the same C parameter and A1 = 1-A2 so its all good

# calculate A for White, Black, Object primes w/in each condition
sumStats <- dat %>% 
  group_by(Subject, Condition, Cue, Probe) %>% 
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
  mutate(C = Gun - (1 - NotGun)) %>% 
  select(-NotGun, -Gun)

# Bind to original sumStats frame
sumStats2 <- full_join(sumStats, control)

# Calculate "Auto"
auto <- sumStats2 %>% 
  spread(key = Probe, value = Accuracy) %>% 
  # Auto = (NotGun-trial errors) / (1 - C)
  mutate(A = (1 - NotGun)/(1 - C)) %>% 
  select(-Gun, -NotGun)

# Bind to data frame
datPDP <- full_join(sumStats2, auto) %>% 
  # Drop redundant columns / rows
  select(-Accuracy, -Probe) %>% 
  distinct()

# save
write.table(datPDP, file="WIT_study4_PDP.txt", sep="\t", row.names=F)

# Check cell N for effect size calculation
datPDP %>% 
  select(Subject, Condition) %>% 
  distinct() %>% 
  group_by(Condition) %>% 
  summarize(n = n())

# Run models ----
whiteA <- lm(A ~ Condition, data = datPDP, subset = Cue == "White")
whiteC <- lm(C ~ Condition, data = datPDP, subset = Cue == "White")
summary(whiteA) 
tes(summary(whiteA)$coefficients[2, 3], 38, 33)
bayesWhiteA <- datPDP %>% 
  filter(Cue == "White") %>% 
  as.data.frame() %>% 
  anovaBF(A ~ Condition, data = ., rscaleFixed = .5)
bayesWhiteA
# White A does not differ across conditions!
# This supports our hypothesis that White-Tool association is meaningless,
# May be attributed to the response-contrast mapping effect
# Let's elaborate with one-sample t-tests to test mu = .5
dat.whiteA1 <- with(datPDP, A[Cue == "White" & Condition == "GunTool"])
dat.whiteA2 <- with(datPDP, A[Cue == "White" & Condition == "GunNeu"])
t.test(dat.whiteA1, mu = 0.5)
t.test(dat.whiteA2, mu = 0.5)
# Interesting -- not significant. I wonder how that looks in our other studies?
# Maybe cause for a follow-up. Maybe not RM at all, but an artifact of ROC curve???

summary(whiteC)
tes(summary(whiteC)$coefficients[2, 3], 38, 33)
# White C does not differ either, indicating not due to shifting control

blackA <- lm(A ~ Condition, data = datPDP, subset = Cue == "Black")
blackC <- lm(C ~ Condition, data = datPDP, subset = Cue == "Black")
summary(blackA)
tes(summary(blackA)$coefficients[2, 3], 38, 33)
summary(blackC)
tes(summary(blackC)$coefficients[2, 3], 38, 33)

# graphics ----
# these histograms would seem to indicate that
# C is holding mostly constant while
# A is shifting fairly dramatically
# Plots of C
datPDP %>% 
  filter(Cue == "Hisp") %>% 
  ggplot(aes(x=C)) +
  geom_histogram(binwidth=.1) +
  facet_grid(Condition~.)

datPDP %>% 
  filter(Cue == "Hisp") %>% 
  ggplot(aes(x = Condition, y = C)) +
  geom_violin() +
  geom_point() +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))

datPDP %>% 
  filter(Cue == "Hisp") %>% 
  ggplot(aes(x = Condition, y = C)) +
  geom_violin() +
  geom_boxplot(width = .5, notch = T) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))

# Plots of A
datPDP %>% 
  filter(Cue == "Hisp") %>% 
  ggplot(aes(x=A)) +
  geom_histogram(binwidth=.1) +
  facet_grid(Condition~.)

datPDP %>% 
  filter(Cue == "Hisp") %>% 
  ggplot(aes(x = Condition, y = A)) +
  geom_violin() +
  geom_point() +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))

datPDP %>% 
  filter(Cue == "Hisp") %>% 
  ggplot(aes(x = Condition, y = A)) +
  geom_violin() +
  geom_boxplot(width = .35, notch = T) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))