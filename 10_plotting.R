# All plots in one place
library(tidyverse)
library(lme4)

dat1 <- read.delim("./clean_wit1.txt")
dat2 <- read.delim("./clean_wit2.txt")
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
dat1 <- dat1 %>% 
  mutate(Subject = as.factor(Subject),
         Condition = substring(ExperimentName, 5),
         ConditionCue = paste(Condition, Cue),
         CueClass = ifelse(ConditionCue %in% gunPrimes, 
                                  "Class_A", 
                                  "Class_B")) %>% 
  filter(Probe.RT < 500)

# fit glmer object
exp1.glmer <- glmer(Probe.ACC ~ Condition * CueClass * Probe + (1|Subject),
           data = dat1, 
           family = "binomial",
           contrasts = list(Condition = "contr.sum",
                            Probe = "contr.sum",
                            CueClass = "contr.sum"),
           control = glmerControl(optimizer="bobyqa"))
summary(exp1.glmer)
Anova(exp1.glmer, type = 3)

# make plot. Note that it appears to be in odds space
# Could expand the grid to make 3×2 grid with missing cells
lsmeans(exp1.glmer, c("Condition", "CueClass", "Probe")) %>% 
  summary() %>% 
  ggplot(aes(x = Probe, y = lsmean, 
             ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_pointrange() +
  facet_grid(Condition ~ CueClass) +
  geom_hline(yintercept = 1, lty = 2)

# Experiment 2 ----
dat2 <- dat2 %>% 
  mutate(Subject = as.factor(Subject),
         Cue = substring(CueType, 1, 4),
         Probe = substring(ProbeType, 1, 4),
         CueClass = ifelse(TrialType %in% c("Hisp.Gun", "Hisp.Tool"), "Hisp", "NonHisp")) %>% 
  filter(Probe.RT <= 500)

# fit model
# Some convergence issues
exp2.glmer <- glmer(Probe.ACC ~ Condition * Probe * CueClass + 
                      (1 + Probe + CueClass|Subject), 
                    data = dat2, 
                    family = "binomial",
                    contrasts = list(Condition = "contr.sum",
                                     Probe = "contr.sum",
                                     CueClass = "contr.sum"),
                    control = glmerControl(optimizer = "Nelder_Mead",
                                           optCtrl = list(maxfun = 20000)))

# Plot cell means and CLs
lsmeans(exp2.glmer, c("Condition", "CueClass", "Probe")) %>% 
  summary() %>% 
  ggplot(aes(x = Probe, y = lsmean, 
             ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_pointrange() +
  facet_grid(Condition ~ CueClass) +
  geom_hline(yintercept = 1, lty = 2)

# May re-order these cells yet
lsmeans(exp2.glmer, c("Condition", "CueClass", "Probe")) %>% 
  summary() %>% 
  ggplot(aes(x = Probe, y = lsmean, 
             ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_pointrange() +
  facet_grid(CueClass ~ Condition) +
  geom_hline(yintercept = 1, lty = 2)

# Experiment 3 ----
dat3 <- mutate(dat3, Subject = as.factor(Subject)) %>% 
  filter(feedbackmask == "fast")

# Fit glmer object
# Note modest failure of convergence
exp3.glmer = glmer(Probe.ACC ~ Condition * CueClass * Probe + 
                 (1 + CueClass * Probe|Subject), 
               contrasts = list(Condition = "contr.sum",
                                CueClass = "contr.sum",
                                Probe = "contr.sum"),
               family = "binomial",
               data = dat3)

# Plot cell means and CLs
lsmeans(exp3.glmer, c("Condition", "CueClass", "Probe")) %>% 
  summary() %>% 
  ggplot(aes(x = Probe, y = lsmean, 
             ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_pointrange() +
  facet_grid(Condition ~ CueClass) +
  geom_hline(yintercept = 1, lty = 2)

lsmeans(exp3.glmer, c("Condition", "CueClass", "Probe")) %>% 
  contrast(list(gun = c(-1, 1, 1, -1, 0, 0, 0, 0)/2)) # still p = .183

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
  filter(SubTrial <= 121,  
         feedbackmask == "fast") 

dat4.summary <- read.delim("acc_wit4.txt", stringsAsFactors = F) %>% 
  mutate(TargetGrp = ifelse(Target == "Gun", "Gun", "NotGun")) %>% 
  mutate_each(funs(as.factor), Subject, Condition, Prime, TargetGrp)

exp4.glmer <- glmer(Probe.ACC ~ Condition * CueClass * Probe + 
               (1 + CueClass * Probe|Subject) + (1|ProbeType) + (1|CueType),
             data = dat4, 
             family = "binomial",
             contrasts = list(Condition = "contr.sum",
                              CueClass = "contr.sum",
                              Probe = "contr.sum"))
summary(exp4.glmer)
Anova(exp4.glmer, type = 3)

exp4.aov <- aov(Probe.ACC ~ Condition * Prime * TargetGrp + Error(Subject/(Prime*TargetGrp)),
            contrasts = list(Condition = "contr.sum",
                             Prime = "contr.sum",
                             TargetGrp = "contr.sum"),
            data = dat4.summary)
summary(exp4.aov)


# Plot cell means and CLs
lsmeans(exp4.glmer, c("Condition", "CueClass", "Probe")) %>% 
  summary() %>% 
  ggplot(aes(x = Probe, y = lsmean, 
             ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_pointrange() +
  facet_grid(Condition ~ CueClass) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_y_continuous("Odds ratio")

# I don't get why the SEs are so huge for this...
lsmeans(exp4.glmer, c("Condition", "CueClass", "Probe")) %>% 
  contrast(list(white.guntool = c(0, 0, 0, 1, 0, 0, 0, -1),
                white.gunneu = c(0, 0, 1, 0, 0, 0, -1, 0)))

lsmeans(exp4.aov, c("Condition", "Prime", "TargetGrp")) %>% 
  summary() %>% 
  ggplot(aes(x = TargetGrp, y = lsmean, 
             ymin = lower.CL, ymax = upper.CL)) +
  geom_pointrange() +
  facet_grid(Condition ~ Prime) +
  scale_y_continuous("Odds ratio", limits = c(0.5, 1))
