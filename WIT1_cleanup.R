# #Scherer Study 1 Cleaning
# #This script deletes blank trials, 
# breaks the subjects into their individual experiments,
# and appends subject race.
# Curiously, 15 subjects seem to be missing.
# Input is WIT_study1_raw.txt
# Output is WIT_study1_refined.txt

library(plyr)
library(dplyr)

# data ingest
dat = read.delim(file = "./rawdata/WIT_study1_raw.txt")

# data cleaning
dat = dat %>% 
  filter(!is.na(SubTrial),
         !is.na(Probe.ACC))
dat = dat %>% 
  select(-ChunkList.Sample)

# Adjust subject ID to be distinct per condition
#"Condition 1" is black & white
#"Condition 2" is neutral & black
#"Condition 3" is neutral & white. Thus,
dat = dat %>% 
  mutate(Subject = Subject + ifelse(ExperimentName == "WIT_neutral_black", 100, 0),
         Subject = Subject + ifelse(ExperimentName == "WIT_neutral_white", 200, 0)) %>% 
  mutate(race = "white")

# label minority participants
minority = c(11, 12, 14, 15, 16, 20, 23, 29, 30, 31, 33, 34, 38, 39, 40, 47, 48,
             c(4, 15, 17, 32, 37, 40, 41, 43, 45) + 100, # Condition 2
             c(6, 14, 18, 20, 28, 32, 34) + 200 # Condition 3
)
dat$race[dat$Subject %in% minority] = "minority"


length(levels(as.factor(dat$Subject))) #121 subjects < (50 + 45 + 41 = 136)
#where are we missing dudes?
tapply(dat$Subject, dat$Subject, FUN=length) #we seem to be missing some subjects for some reason?
missing = c(7:9, 11:12, 34, 106:107, 111:112, 206:207, 210:212)
length(missing) #the mysteriously absent 15 subjects...
#We'll figure out the missing subjects later, I guess. 
#Let's see what the analysis looks like without them

# Exclude those participants who are not significantly better than chance
# not sure whether alpha = .05 or alpha = .01 (or any difference)

mean.acc = dat %>% 
  group_by(Subject) %>% 
  summarize(accuracy = mean(Probe.ACC, na.rm = T),
            trials = n())
# Are subjects with <20% accuracy performing reversed mapping, 
#  or are they not responding?
hist(mean.acc$accuracy)
# What's necessary accuracy for p < .01 above chance in 256 trials?
qbinom(.99, 256, .5, lower.tail = T)/256
# Must be 57.4% accurate to ride
hist(mean.acc$accuracy[mean.acc$accuracy > .3])
abline(v = .574, col = 'red')
bad = mean.acc$Subject[mean.acc$accuracy < .574]

# Filter out bad-accuracy subjects
dat = dat %>% 
  filter(!(Subject %in% bad))

# Filter out minority (black?) subjects
# TODO: Can I distinguish black from general minority status?
dat = dat %>%
  filter(race != "minority")

write.table(dat, file="clean_wit1.txt", sep="\t", row.names=F)
