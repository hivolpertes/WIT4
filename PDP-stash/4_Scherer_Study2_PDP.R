setwd(choose.dir())
dat = read.delim(file="WIT_study2_SASdat.txt")

# let's define Automatic in terms of bias towards gun
# e.g. A=1 means always automatic priming of gun by prime
# whereas A=0 means always automatic priming of tool by prime

# calculate A for White, Black, Object primes w/in each condition

# from Stewart & Payne, 2008:
# C = P(correct | congruent) - P(stereotypic error | incongruent)
# A = P(stereotypic error | incongruent) / (1-C)
subBlackHisp = unique(dat$sub[dat$Condition == "BlackHisp"])
subNeutHisp = unique(dat$sub[dat$Condition == "neutHisp"])
subWhiteHisp = unique(dat$sub[dat$Condition == "WhiteHisp"])

datPDP = dat[(1:nrow(dat) %% 2 == 1),]
C = dat$acc[dat$ProbeType == "weapon"] + dat$acc[dat$ProbeType == "tool"] - 1
A = (1 - dat$acc[dat$Probe == "tool"]) / (1 - C)
head(dat[dat$Probe == "weapon",])
head(dat[dat$Probe == "tool",])

datPDP$C = C
datPDP$A = A

# export for SAS
write.table(datPDP, file="datPDP2.txt", sep="\t", row.names=F)

require(ggplot2)
# these histograms would seem to indicate that
# C is holding mostly constant while
# A is shifting fairly dramatically
ggplot(datPDP, aes(x=C)) +
  geom_histogram(binwidth=.1) +
  facet_wrap(~CueType*Condition, nrow=3)

ggplot(datPDP, aes(x=A)) +
  geom_histogram(binwidth=.1) +
  facet_wrap(~CueType*Condition, nrow=3)

require(lme4); require(car)
# the general model fails to converge probably due to
# insufficient degrees of freedom / missing cells
# it's a pain but I'll need to run more specific contrasts...
datPDPHispanic = datPDP[datPDP$CueType == "hispanic",]
datPDPHispanic$sub = as.factor(datPDPHispanic$sub)


report = function(model) {print(summary(model)); print(Anova(model, type=3))}

#yes! take it to the blue line
HispanicA1 = lm(A ~ Condition, dat=datPDPHispanic)
HispanicC1 = lm(C ~ Condition, dat=datPDPHispanic)
report(HispanicA1)
report(HispanicC1)

ggplot(datPDP, aes(x=Condition, y=A)) +
  geom_boxplot(notch=T)
ggplot(datPDP, aes(x=Condition, y=C)) +
  geom_boxplot(notch=T)

# we'll return to this later for parameter estimates & plots.
