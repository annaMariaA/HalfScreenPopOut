# setwd÷("C:/Users/r02al13/Desktop/4ExperiemntsSubmitted/LineSegmEasyDiff")
# fixdat = readRDS(file="pilotData/processedFixData.Rda")
fixdat = readRDS(file="processedFixData.Rda")

fixdat = fixdat[fixdat$targSide=="absent" ,]
fixdat = fixdat[fixdat$hemiType=="Unmodified",]
# fixdat = fixdat[fixdat$var=="parallel",]
# 

fixPerTrial = aggregate(data=fixdat, fixNum ~ subj+var+trial, FUN="length")
# numFix = aggregate(data=fixdat, fixNum ~ subj, FUN="median")


library(ggplot2)

ggplot(numFix, aes(x=subj, y=fixNum, fill=var)) + geom_boxplot()

fixPerTrial$fixNum = (fixPerTrial$fixNum-1)/2


library(dplyr)

aggData = (fixPerTrial
  %>% group_by(subj, var) 
    %>% summarise(
     meanNumFix=mean(fixNum), 
     nTrials=length(fixNum),
     stdev = sd(fixNum),
     sterr = stdev/sqrt(nTrials),
     lower = meanNumFix - 1.96 * sterr,
     upper = meanNumFix + 1.96 * sterr,
     ci95range = upper-lower))


plt = ggplot(aggData, aes(x=subj, y=meanNumFix, ymin=lower, ymax=upper, colour=var))
plt = plt + geom_errorbar() + geom_point()
ggsave("subjMean95ci.pdf")

plt = ggplot(aggData, aes(x=ci95range, fill=var)) + geom_density(alpha=0.5)
ggsave("range95ci.pdf")