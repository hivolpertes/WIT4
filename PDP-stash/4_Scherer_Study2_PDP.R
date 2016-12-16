library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
# How to get effect sizes?
library(afex)
# library(ez)

dat <- read.delim(file="clean_wit2.txt") %>% 
  mutate(Probe = ifelse(grepl("Gun", TrialType), "Gun", "Tool"),
         Cue = ifelse(grepl("Black", TrialType), "Black",
                      ifelse(grepl("Hisp", TrialType), "Hisp",
                             ifelse(grepl("Neut", TrialType), "Neut",
                                          "White"))))

# let's define Automatic in terms of bias towards gun
# e.g. A=1 means always automatic priming of gun by prime
# whereas A=0 means always automatic priming of tool by prime
# I did the algebra and you get the same C parameter and A1 = 1-A2 so its all good

# Thus, we're not conditioning on experiment, just 

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
  # Drop redundant columns / rows
  select(-Accuracy, -Probe) %>% 
  distinct()

# save
write.table(datPDP, file="WIT_study2_PDP.txt", sep="\t", row.names=F)

# Check cell N for effect size calculation
datPDP %>% 
  select(Subject, Condition) %>% 
  distinct() %>% 
  group_by(Condition) %>% 
  summarize(n = n())

# Run models ----
summary(aov(A ~ Condition, data = datPDP, subset = Cue == "Hisp"))
esci(.2506, 1.3230, 2, 69, .9)
summary(aov(C ~ Condition, data = datPDP, subset = Cue == "Hisp"))
esci(.1448, 2.7944, 2, 69, .9)

hispanicA <- lm(A ~ Condition, data = datPDP, subset = Cue == "Hisp")
hispanicC <- lm(C ~ Condition, data = datPDP, subset = Cue == "Hisp")
summary(hispanicA)
tes(summary(hispanicA)$coefficients[2, 3], 20, 25)
tes(summary(hispanicA)$coefficients[3, 3], 20, 27)
summary(hispanicC)
tes(summary(hispanicC)$coefficients[2, 3], 20, 25)
tes(summary(hispanicC)$coefficients[3, 3], 20, 27)

TukeyHSD(aov(A ~ Condition, data = datPDP, subset = Cue == "Hisp"))
TukeyHSD(aov(C ~ Condition, data = datPDP, subset = Cue == "Hisp"))

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

# Table
datPDP %>% group_by(Condition, Cue) %>% 
  summarize(A = round(mean(A), 2), 
            C = round(mean(C), 2)) 

