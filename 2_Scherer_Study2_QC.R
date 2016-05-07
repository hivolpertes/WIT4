#Scherer Study 2 Quality Control
#I think the best single criterion is to eliminate all subjects with accuracy not
#significantly greater than .5

#Note that subjects perform 
# [32 trials/subblock * 3 subblocks / block * 3 blocks] = 288 trials each

#Data in: WIT_study2_refined.txt
#Data out: WIT_study2_badsubs.txt, a vector of bad subject #s

#setwd(choose.dir())
dat=read.delim(file="WIT_study2_refined.txt", sep="\t")

#install.packages('reshape2')
require(reshape2)
molten = melt(dat, 
              id.var=c("sub", "mapping", "Condition", "Trial", "CueType", "ProbeType",
                       "feedduration", "TrialType", "race"
              )
) 

#"check" is overall accuracy and RT.
#"d" is by-cell accuracy and RT
check = dcast(molten, sub + race + Condition ~ variable, fun.aggregate=mean)
hist(check$Probe.RT); 
hist(check$Probe.ACC, breaks=20); abline(v=.5, col='red'); abline(v=.56, col='orange')

#how accurate must you be to be significantly (p=.05) greater than chance?
#calculate beta-crit given a particular alpha
alpha = .01 #specify alpha here: I'm considering .01 instead of .05
n=288
se=sqrt(.5*(1-.5)/n)
z.crit= qnorm(1-alpha)
p.null= .5
p.crit=se*z.crit+p.null
p.crit.lo=p.null-se*z.crit

p.crit

#thus participants must attain at least 56.9% accuracy to be sig. better than chance.
check[check$Probe.ACC < p.crit, ]
bad.2=check[check$Probe.ACC < p.crit, 1]

#let's inspect subjects previously excluded for non-response or fastRT
#that still had accuracy above chance

#how do people in / out of bad.2 compare?
hist(check$Probe.RT[check$sub %in% bad.2], main="bad accuracy subjects RT histogram", xlim=c(0, 600))
hist(check$Probe.RT[!(check$sub %in% bad.2)], main="ok accuracy subjects RT histogram", xlim=c(0, 600))
hist(check$Probe.ACC[check$sub %in% bad.2], main="bad accuracy subjects ACC histogram", xlim=c(0,1))
hist(check$Probe.ACC[!(check$sub %in% bad.2)], main="ok accuracy subjects ACC histogram", xlim=c(0,1))

#maybe bad.2 is really all the exclusion criteria I need...
#print("Thou must savest vector 'bad.2' manually, my lord.")
write(bad.2, file="WIT_study2_badsubs_2.txt", sep="\t")