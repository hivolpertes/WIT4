#again I think the best metric might be just
#Exclude those participants who are not significantly better than chance
#maybe I should tighten up the alpha on that judgement to a=.01 rather than a=.05


setwd("C:/data_2014/Scherer_data_post/Study1")
dat=read.delim(file="WIT_study1_refined.txt", sep="\t")
#bad=scan(file="WIT_study1_badsubs.txt", sep="\t")

#calculate beta-crit given a particular alpha
alpha = .01 #specify alpha here: I'm considering .01 instead of .05
n=64*4
se=sqrt(.5*(1-.5)/n)
z.crit= qnorm(1-alpha)
p.null= .5
p.crit=se*z.crit+p.null

p.crit

require(reshape2)
cols=colnames(dat)
dvs=c("Probe.ACC", "Probe.RT")
ids=colnames(dat)[!(colnames(dat) %in% dvs)]
molten=melt(dat, id.var=ids)
check = dcast(molten, sub + ExperimentName + mapping + race ~ variable, 
             fun.aggregate=mean)
hist(check$Probe.ACC)
# bad.acc stores list of subjects with unacceptible accuracy
bad.acc=check$sub[check$Probe.ACC<=p.crit]

#i'll stand by my decision to keep it to those ps with accuracy sig. better than chance.
#maybe one day I'll make a function to simply print off their p-value for p.hat =< .5.

#print("Thou must save manually, my lord!")
write(bad.acc, file="WIT_study1_badsubs.txt", sep="\t")
