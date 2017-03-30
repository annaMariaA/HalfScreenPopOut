library(dplyr)
library(ggplot2)
library(binom)
library(lme4)

fixdat = readRDS(file="../data/processedFixData.Rda")
fixdat$subj = as.character(fixdat$subj)

cbPalette <- c("#56B4E9", "#E69F00")
fixdat$order = paste(fixdat$version, fixdat$completed, sep="")
fixdat$order[which(fixdat$order=="T 2")] = "UT"
fixdat$order[which(fixdat$order=="N 2")] = "TU"
fixdat$order[which(fixdat$order=="T 1")] = "TU"
fixdat$order[which(fixdat$order=="N 1")] = "UT"

fixdat$order = as.factor(fixdat$order)


fixdat$subj = as.factor(paste(fixdat$order, fixdat$subj))

# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "homo"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "hetro"
fixdat$side = as.factor(fixdat$side)

aggData = (filter(fixdat, 
  side!="central", 
  fixNum<6, 
  fixNum>1,
  # fixOn<2000, 
  targSide=="absent",
  condition=="half") 
  %>% group_by(fixNum, subj, version) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper, colour=version))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~subj, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side")
ggsave("../plots/FixXsideByFixNumAndSubjExCentral.pdf", width=9, height=4)

# get mean person plot
aggData2 = (filter(aggData, fixNum<6, fixOn<4000) 
  %>% group_by(fixNum, version, completed) 
    %>% summarise(
     mPropHetro=mean(propHetro), 
     stddev = sd(propHetro),
     stderr=stddev/sqrt(12),
    lower=mPropHetro-1.96*stderr,
    upper=mPropHetro+1.96*stderr))

plt = ggplot(aggData2, aes(x=fixNum,y=mPropHetro, ymin=lower, ymax=upper))
plt = plt + geom_path() + geom_errorbar()# + geom_hline(y=0.5)
plt = plt + theme_bw() +facet_grid(completed~version)
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side", breaks=c(0,0.5,1), limits=c(0,1))
plt = plt + scale_x_continuous('fixation number', breaks=c(2,4,6,8,10))
ggsave("../plots/meanPersonSide.pdf", width=4, height=4)
ggsave("../plots/meanPersonSide.jpg",dpi=600, width=6, height=4)



#  now we want to compare the strategy for trials which timed out, v those that didn't.
fixdat$timedOut = FALSE
for (s in levels(fixdat$subj))
{
  for (t in levels(fixdat$trial))
  {
    
   
    trlDat = filter(fixdat, subj==s, trial==t, version=="T")
if (nrow(trlDat)>0)
      {fixdat$timedOut[which(fixdat$subj==s & fixdat$trial==t, version=="T")] = rep(max(trlDat$fixOn)>4000, nrow(trlDat))}
}
}


aggData = (filter(fixdat, version=="T", side!="central", fixNum<6, fixNum>1, targSide=="absent", condition=="half") 
  %>% group_by(fixNum, subj, timedOut) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper, colour=timedOut))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~subj, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side")
ggsave("../plots/FixXtimedOut.pdf", width=9, height=4)


aggregate(trial ~ subj+targSide)
