# mini meta
library(plyr)
library(tidyverse)
library(lme4)
library(lsmeans)
library(metafor)


dat1 <- read.delim("./clean_wit1.txt") 
#dat2 <- read.delim("./clean_wit2.txt")
dat3 <- read.delim("./clean_wit3.txt")
dat4 <- read.delim("./clean_wit4.txt")

# Experiment 1 ----
# Reduce 3 prime categories to 2 per hypothesis
gunPrimes = c("black_white Black", "neutral_black Black", 
              "neutral_white White")
toolPrimes = c("black_white White", "neutral_black Neutral", 
               "neutral_white Neutral")
# Prepare data
# Convert Subject to factor, prettify ExperimentName,
# combine condition with prime to prepare 2-category analysis,
# reduce 3 prime categories to 2,
# and discard too-slow trials
dat1 <-  dat1 %>% 
  mutate(TrialType = dplyr::recode(TrialType, 
                                   BlackGun = "Black.Gun", WhiteGun = "White.Gun", NeutralGun = "Neutral.Gun",
                                   BlackTool = "Black.Tool", WhiteTool = "White.Tool", NeutralTool = "Neutral.Tool")) %>% 
  separate(TrialType, c("Cue", "Probe"), sep = "\\.") %>% 
  mutate(Condition = substring(ExperimentName, 5)) %>% # prune "WIT_" prefix
  mutate(ConditionCue = paste(Condition, Cue))

dat1 <- dat1 %>% 
  mutate(Subject = as.factor(Subject),
         Condition = substring(ExperimentName, 5),
         ConditionCue = paste(Condition, Cue),
         CueClass = ifelse(ConditionCue %in% gunPrimes, 
                           "Class_A", 
                           "Class_B")) %>% 
  filter(Probe.RT < 500)

# fit glmer object
exp1.glmer <- glmer(Probe.ACC ~ CueClass * Probe + 
                      (1 + CueClass * Probe|Subject) + (1|CueType) + (1|ProbeType),
                    data = dat1, 
                    subset = Condition == "black_white",
                    family = "binomial",
                    contrasts = list(Probe = "contr.sum",
                                     CueClass = "contr.sum"),
                    control = glmerControl(optimizer="bobyqa"))
summary(exp1.glmer)
Anova(exp1.glmer, type = 3)

lsmeans(exp1.glmer, c("CueClass", "Probe")) %>% 
  summary()

c1 <- lsmeans(exp1.glmer, c("CueClass", "Probe")) %>% 
  contrast(list(WhiteGun.vs.WhiteTool = c(0, -1, 0, 1)))


# Experiment 3 ----
dat3 <- mutate(dat3, Subject = as.factor(Subject)) %>% 
  filter(feedbackmask == "fast")

# Fit glmer object
# Note modest failure of convergence
# Had to drop interaction slope due to negative hessian
exp3.glmer = glmer(Probe.ACC ~ CueClass * Probe + 
                     (1 + CueClass + Probe|Subject) + (1|CueType) + (1|ProbeType), 
                   contrasts = list(CueClass = "contr.sum",
                                    Probe = "contr.sum"),
                   family = "binomial",
                   data = dat3,
                   subset = Condition == "GunTool")
summary(exp3.glmer)

lsmeans(exp3.glmer, c("CueClass", "Probe")) %>% 
  summary()

c3 <- lsmeans(exp3.glmer, c("CueClass", "Probe")) %>% 
  contrast(list(WhiteGun.vs.WhiteTool = c(0, -1, 0, 1)))

# Experiment 4 ----
dat4 <- dat4 %>% 
  mutate(Subject = as.factor(Subject), # Subject as factor
         # Combine individual stimuli to a type
         ProbeClass = mapvalues(ProbeType,
                                from = c("absneut3.bmp", "banana.bmp", "giraffe.bmp", "gripstrength.bmp",
                                         "TOOL1.bmp", "TOOL2.bmp", "TOOL3.bmp", "TOOL4.bmp",
                                         "WEAPON1.bmp", "WEAPON2.bmp", "WEAPON3.bmp", "WEAPON4.bmp"),
                                to = rep(c("Neutral", "Tool", "Weapon"), each = 4)),
         CueClass = substr(CueType, 1, 5),
         Probe = ifelse(ProbeClass == "Weapon", "Weapon", "Other")) %>% 
  # Filter out trials after the 120th and too-slow trials
  filter(#SubTrial <= 121,  
    feedbackmask == "fast") 

exp4.glmer <- glmer(Probe.ACC ~ CueClass * Probe + 
                      (1 + CueClass * Probe|Subject) + (1|CueType) + (1|ProbeType), 
                    contrasts = list(CueClass = "contr.sum",
                                     Probe = "contr.sum"),
                    family = "binomial",
                    data = dat4,
                    subset = Condition == "GunTool")
summary(exp4.glmer)

lsmeans(exp4.glmer, c("CueClass", "Probe")) %>% 
  summary()

c4 <- lsmeans(exp4.glmer, c("CueClass", "Probe")) %>% 
  contrast(list(WhiteGun.vs.WhiteTool = c(0, 1, 0, -1)))

# Fetch the three White-Gun - White-Tool contrasts
dat.contrast <- bind_rows(summary(c1), summary(c3), summary(c4))

rma.fit <- rma(yi = estimate,
               sei = SE,
               data = dat.contrast)
summary(rma.fit)
forest(rma.fit, main = "Multilevel model")

# RM ANOVA
# Exp 1
dat1.acc <- read.delim("acc_wit1.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject),
         Condition = substring(ExperimentName, 5))

exp1.anova <- dat1.acc %>% 
  filter(Condition == "black_white") %>% 
  aov(Probe.ACC ~ Cue * Probe + Error(Subject/(Cue*Probe)),
      contrasts = list(Cue = "contr.sum",
                       Probe = "contr.sum"),
      data = .)

summary(exp1.anova)

lsmeans(exp1.anova, c("Cue", "Probe"))

c1.anova <- lsmeans(exp1.anova, c("Cue", "Probe")) %>% 
  contrast(list(WhiteGun.vs.WhiteTool = c(0, -1, 0, 1)))

# Exp 3
dat3.acc <- read.delim("acc_wit3.txt", stringsAsFactors = F) %>% 
  mutate(Subject = as.factor(Subject),
         Probe = ifelse(ProbeClass == "WEAP", "Gun", "Not-Gun")) %>% 
  mutate(CueClass = as.factor(CueClass),
         Probe = as.factor(Probe),
         Condition = as.factor(Condition))

exp3.anova <- dat3.acc %>% 
  filter(Condition == "GunTool") %>% 
  aov(Probe.ACC ~ CueClass * Probe + Error(Subject/(CueClass*Probe)),
      data = .,
      contrasts = list(CueClass = "contr.sum",
                       Probe = "contr.sum"))

summary(exp3.anova)

lsmeans(exp3.anova, c("CueClass", "Probe"))

c3.anova <- lsmeans(exp3.anova, c("CueClass", "Probe")) %>% 
  contrast(list(WhiteGun.vs.WhiteTool = c(0, -1, 0, 1)))

# Exp 4
dat4.acc <- read.delim("acc_wit4.txt", stringsAsFactors = F) %>% 
  mutate(TargetGrp = ifelse(Target == "Gun", "Gun", "NotGun")) %>% 
  mutate_each(funs(as.factor), Subject, Condition, Prime, TargetGrp)

exp4.anova <- aov(Probe.ACC ~ Prime * TargetGrp + Error(Subject/(Prime*TargetGrp)),
                  contrasts = list(Prime = "contr.sum",
                                   TargetGrp = "contr.sum"),
                  data = dat4.acc,
                  subset = Condition == "GunTool")
summary(exp4.anova)

lsmeans(exp4.anova, c("Prime", "TargetGrp"))

c4.anova <- lsmeans(exp4.anova, c("Prime", "TargetGrp")) %>% 
  contrast(list(WhiteGun.vs.WhiteTool = c(0, -1, 0, 1)))

# Fetch contrasts and meta-analyze
dat.contrast.anova <- bind_rows(summary(c1.anova),
                                summary(c3.anova),
                                summary(c4.anova))

rma.fit.anova <- rma(yi = estimate,
                     sei = SE,
                     data = dat.contrast.anova)
summary(rma.fit.anova)
forest(rma.fit.anova, main = "ANOVA model")
