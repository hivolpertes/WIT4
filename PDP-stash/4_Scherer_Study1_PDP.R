library(plyr)
library(dplyr)

dat <- read.delim(file="clean_wit1.txt")

# let's define Automatic in terms of bias towards gun
# e.g. A=1 means always automatic priming of gun by prime
# whereas A=0 means always automatic priming of tool by prime

# calculate A for White, Black, Object primes w/in each condition

subBlackWhite = unique(dat$sub[dat$ExperimentName == "WIT_black_white"])
subNeutBlack = unique(dat$sub[dat$ExperimentName == "WIT_neutral_black"])
subNeutWhite = unique(dat$sub[dat$ExperimentName == "WIT_neutral_white"])

sumStats <- dat %>% 
  group_by(Subject, ExperimentName, Cue, Probe) %>% 
  summarise(Accuracy = mean(Probe.ACC))

# Black-Gun and White-Tool are "congruent" for BlackWhite condition
#sumStats$congruency[] 

# from Stewart & Payne, 2008:
# C = P(correct | congruent) - P(stereotypic error | incongruent)
# A = P(stereotypic error | incongruent) / (1-C)
C <- sumStats$Probe.ACC[dat$Probe == "Gun"] - (1 - sumStats$Probe.ACC[dat$Probe == "Tool"])
A <- (1 - dat$Probe.ACC[dat$Probe == "Tool"]) / (1 - C)
head(dat[dat$Probe == "Gun",])
head(dat[dat$Probe == "Tool",])

# make table w/ just PDP estimates
datPDP = dat[(1:nrow(dat) %% 2 == 1), 1:5]
datPDP$C = C
datPDP$A = A
#View(datPDP)

# save
write.table(datPDP, file="WIT_study1_PDPdata.txt", sep="\t", row.names=F)


##############
# here be old analysis

require(ggplot2)
# these histograms would seem to indicate that
# C is holding mostly constant while
# A is shifting fairly dramatically
ggplot(datPDP, aes(x=C)) +
  geom_histogram(binwidth=.1) +
  facet_wrap(~Prime*Condition, nrow=3)

ggplot(datPDP, aes(x=A)) +
  geom_histogram(binwidth=.1) +
  facet_wrap(~Prime*Condition, nrow=3)

require(lme4); require(car)
# the general model fails to converge probably due to
# insufficient degrees of freedom / missing cells
# it's a pain but I'll need to run more specific contrasts...
datPDPBlack = datPDP[datPDP$Prime == "Black",]
datPDPWhite = datPDP[datPDP$Prime == "White",]
datPDPNeutral=datPDP[datPDP$Prime == "Neutral",]
datPDPBlack$Subject = as.factor(datPDPBlack$Subject)
datPDPWhite$Subject = as.factor(datPDPWhite$Subject)
datPDPNeutral$Subject = as.factor(datPDPNeutral$Subject)


report = function(model) {print(summary(model)); print(Anova(model, type=3))}

#yes! take it to the blue line
BlackA1 = lm(A ~ Condition, dat=datPDPBlack)
BlackC1 = lm(C ~ Condition, dat=datPDPBlack)
summary(BlackA1)
summary(BlackC1)

WhiteA1 = lm(A ~ Condition, dat=datPDPWhite)
WhiteC1 = lm(C ~ Condition, dat=datPDPWhite)
summary(WhiteA1)
summary(WhiteC1)

NeutralA1 = lm(A ~ Condition, dat=datPDPNeutral)
NeutralC1 = lm(C ~ Condition, dat=datPDPNeutral)
summary(NeutralA1)
summary(NeutralC1)

# we'll return to this later for parameter estimates & plots.
