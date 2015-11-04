library(dplyr)
library(ggplot2)
library(binom)

# # setwd("~/Desktop/HalfPopOutAnalysisToday")
# setwd("~/Documents/HalfPopOutAnalysisToday")
 setwd("C:/Users/r02al13/Documents/GitHub/HalfScreenPopOut")
fixdat = readRDS(file="data/processedFixData.Rda")
fixdat = (filter(fixdat, subj!=4, subj!=15 ,) 
cbPalette <- c("#56B4E9", "#E69F00")
library(lme4)
library(ggplot2)
library(scales)
#library(bear)
#library(boot)





# Plot distribution of fixation x-coord by fixation number

m = ggplot(
	data=filter(fixdat, fixNum<13, fixX<1024, fixX>0, targSide=="absent"), 
	aes(x = fixX))
m = m + geom_histogram(fill="purple", binwidth=64) 
m = m + facet_wrap(~fixNum, scales="free_y", nrow=4)
m = m + theme_bw()
m = m + scale_x_continuous(name="fixation horizontal postition", breaks=c(0,512,1024), expand=c(0,0))
ggsave("plots/FixXpostByFixNum.pdf", width=9, height=9)
ggsave("plots/FixXpostByFixNum.jpg",dpi=600, width=9, height=8)

# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "homo"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "hetro"
fixdat$side = as.factor(fixdat$side)

aggData = (filter(fixdat, side!="central", fixNum<11, fixNum>1, targSide=="absent") 
  %>% group_by(fixNum, subj) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper))
plt = plt + geom_point() + geom_path(se=F) + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~subj)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side")
ggsave("plots/FixXsideByFixNumAndSubjExCentral.pdf", width=9, height=9)
ggsave("plots/FixXsideByFixNumAndSubjExCentral.jpg",dpi=600, width=9, height=9)

# get mean person plot
aggData2 = (filter(aggData, fixNum<11) 
  %>% group_by(fixNum) 
    %>% summarise(
     mPropHetro=mean(propHetro), 
     stddev = sd(propHetro),
     stderr=stddev/sqrt(12),
    lower=mPropHetro-1.96*stderr,
    upper=mPropHetro+1.96*stderr))

plt = ggplot(aggData2, aes(x=fixNum,y=mPropHetro, ymin=lower, ymax=upper))
plt = plt + geom_path() + geom_errorbar()# + geom_hline(y=0.5)
plt = plt + theme_bw() 
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side", breaks=c(0,0.5,1), limits=c(0,1))
plt = plt + scale_x_continuous('fixation number', breaks=c(2,4,6,8,10))
ggsave("plots/meanPersonSide.pdf", width=6, height=4)
ggsave("plots/meanPersonSide.jpg",dpi=600, width=6, height=4)

fixWasteDat = (filter(fixdat, side!="central")
%>% group_by(subj, targSide, trial)
%>% summarise(
 fixNumTotal =length(fixNum),
 fixNumHomo = sum(side=="homo")))

rtdat = readRDS(file="data/processedRTandAccData.Rda")

fixWasteDat = merge(fixWasteDat, rtdat)

plt = ggplot(filter(fixWasteDat, targSide=="absent"), aes(x=fixNumHomo, y=RT)) 
plt = plt + geom_point(colour="gray") + geom_smooth(method="lm", se=F, colour="black")
plt = plt + theme_bw()
plt = plt + scale_x_continuous(name="num. fix. to homogeneous side")
plt = plt + scale_y_continuous(name="reaction time (seconds)")
ggsave("plots/trialByTrialCor.pdf", height=3.2, width=3.2)
ggsave("plots/trialByTrialCor.jpg",dpi=600, height=3.2, width=3.2)

library(lme4)
m = lmer(data=filter(fixWasteDat, targSide=="absent"), fixNumTotal~fixNumHomo + (fixNumHomo|subj))
summary(m)

m = lmer(data=filter(fixWasteDat, targSide=="absent"), 
	RT~fixNumHomo + (fixNumHomo|subj))
summary(m)



indivDiffCorr = (filter(fixdat, side!="central", fixNum<11, fixNum>1, targSide=="absent") 
  %>% group_by(subj) 
    %>% summarise(
     propHomo=mean(side=="homo")))

hetroRT = (filter(rtdat, targSide=="hetrogeneous", acc==1, is.finite(RT)) 
  %>% group_by(subj) 
    %>% summarise(
     medianRT_hetro = median(RT)))

homoRT = (filter(rtdat, targSide=="homogeneous", acc==1, is.finite(RT)) 
  %>% group_by(subj) 
    %>% summarise(
     medianRT_homo = median(RT)))

dat = merge(homoRT, merge(hetroRT, indivDiffCorr))
write.table(dat, "data/correlationData14pps10fix.txt", sep=",")

plt = ggplot(dat, aes(x=propHomo, y=medianRT_hetro)) + geom_point(colour="grey") + geom_smooth(method="lm", se=F, colour="black")
plt = plt + theme_bw()
plt = plt + scale_x_continuous(name="prop. homogeneous fixations (absent)", limits=c(0,1))
plt = plt + scale_y_continuous(name="median RT (present)")
plt = plt + geom_text(data=NULL, label="A", x = 0, y=8 )
ggsave("plots/personByPersonCor.pdf", height=3.2, width=3.2)
