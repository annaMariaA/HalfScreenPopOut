
library(dplyr)
# TODO: check subjects to replace!
 setwd("C:/Users/Anna/Documents/GitHub/HalfScreenPopOut/DeadlinExp")

saccInfo <- function(trialDat)
{
	# this is a funtion that calculates saccade amplitude and angle for a sequenece of fixations in a  trial
	nFix = max(trialDat$fixNum)
	saccInfo  = trialDat
	saccAmp2 = vector()
	theta = vector()
	for (f in 1:(nFix-1))
	{
		dx = trialDat$fixX[f] - trialDat$fixX[f+1]
		dy = trialDat$fixY[f] - trialDat$fixY[f+1]
		saccAmp2[f] 	= dx^2 + dy^2
		theta[f] 		= atan2(dx, dy)
	}
	saccAmp2[nFix] = NaN
	theta[nFix]    = NaN
	saccInfo$amp   = sqrt(saccAmp2)
	saccInfo$ang   = theta

	return(saccInfo)
}


subjectsToRemove =c( 3,6,10,11,16,31,30)# one participant familiar with the rationale/hypothesis, 4 completed previous versions and remaining two pps data missing
# max fixation duration - remove trial if it is exceeded 
maxFixDur = 2000

# read in reaction time and acc data:
# this will allow us to remove fixation data for incorrect trials
print("Processing RT and Acc data")
#dat <- read.csv("../data/RtAcc32.txt", sep="\t")
dat <- read.csv("data/RtAcc32.txt", sep="\t")
names(dat) = c("subj","version", "completed", "trialNum","easySide", "targPresent","key", "targSide", "RT", "acc", "var", "con")

# dat$version = factor(paste(dat$version, dat$completed))
# levels(dat$version) = c('untimedFirst', 'untimedSecond', 'timedFirst','timedSecond')
dat = select(dat, subj, version, trialNum, easySide, targPresent, targSide, RT, acc, var, con)

# Turn categorical data into factor
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
levels(dat$targSide) = c('left', 'right', 'absent')
dat$easySide = as.factor(dat$easySide)
levels(dat$easySide) = c("left", "right","x")

dat$var = as.factor(dat$var)
levels(dat$var) = c("hetero", "homo", "x")
dat$con = as.factor(dat$con)
levels(dat$con) = c("full", "half")

# remove some subjects

dat$subj = as.factor(dat$subj)
dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = as.factor(dat$subj)


# refdefine targSide relative to easySide
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hetrogeneous", "homogeneous", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"

# make a new, tidier version of dataframe only including the stuff we want!
rtdat = data.frame(subj=dat$subj, version=dat$version, trial=dat$trial, targSide=dat$targSideRel, RT=dat$RT, acc=dat$acc, easySide=dat$easySide,var=dat$var, condition=dat$con )
# we don't want to be looking at RTs for incorrect trials
rtdat$RT[rtdat$acc==0] = NaN

levels(rtdat$version) = c("N", "T")

rtdat$trial = paste(rtdat$trial, rtdat$version, sep="")



# save!!!
saveRDS(rtdat,file="data/processedRTandAccData.Rda")
#saveRDS(rtdat,file="../data/processedRTandAccData.Rda")

# remove data for now
rm(dat, rtdat)


#############################
# now read in fixation data #
#############################

print("Processing Fix data...")
#dat <- read.csv("../data/Fix.txt", header=T, sep="\t",
dat <- read.csv("data/Fix32.txt", header=T, sep="\t",
	colClass = c(
		"subNum"="factor",
		"version"  ="factor", 
		"completed" ="factor",  
		"trialNo"="numeric", 
		"fixNo"="numeric",    
		"xFix" = "numeric",
		"yFix" = "numeric",
		"fixStartTime" = "numeric",
		"fixEndTime" = "numeric",
		"targPresent" = "factor",
		"targSide" = "factor",
		"easySide" = "factor",
		"var" = "factor",
		"con"="factor"))
names(dat) = c("subj", "version", "completed", "trialNum","fixNum", "fixX","fixY","fixOn","fixOff", "targPresent", "targSide", "easySide","var","condition")


levels(dat$targPresent) = c("absent", "present")
levels(dat$targSide) = c("left", "right", "absent")
levels(dat$easySide) = c("left", "right","x")

levels(dat$version) = c("N", "T")
dat$trialNum = paste(dat$trialNum, dat$version, sep="")

dat = select(dat, subj,  version, completed, trialNum, fixNum, easySide, fixX, fixY, fixOn, fixOff, targPresent, targSide,var, condition)

# refdefine targSide relative to easySide - ie, hetrogeneous array always on left
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hetrogeneous", "homogeneous", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"

dat$var = as.factor(dat$var)
levels(dat$var) = c("hetero", "homo", "x")
dat$condition = as.factor(dat$condition)
levels(dat$condition) = c("full", "half")

# calcualte fixation durations
dat$fixDur = with(dat, fixOff - fixOn)

# remove unwanted participants

dat$subj = as.factor(dat$subj)
dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = as.factor(dat$subj)


# #we want to filter out all incorrect trials!
 print("...removing fixation for incorrect trials and fix.dur exceptions")
accdat = readRDS(file="data/processedRTandAccData.Rda")
#accdat = readRDS(file="../data/processedRTandAccData.Rda")
 dat$acc = 0
 for (s in levels(dat$subj))
 {
 	subjDat = filter(dat, subj==s)
      subjDat$trialNum = factor(subjDat$trialNum)
 	for (t in levels(subjDat$trialNum))
 	{
  		j = which(accdat$trial==t & accdat$subj==s)
  		idx = which(dat$subj==s & dat$trialNum==t)
      	if (accdat$acc[j]==1 & max(dat$fixDur[idx]) <= maxFixDur )
      	{
      		dat$acc[idx] = 1
      	}
      }
  }
  print(paste("... keeping ", 100*mean(dat$acc), "% of fixations"))
  dat = filter(dat, acc==1)

saveRDS(dat,file="data/processedFixData.Rda")
#saveRDS(dat,file="../data/processedFixData.Rda")
 rm(dat)

fixdat = readRDS(file="data/processedFixData.Rda")
#fixdat = readRDS(file="../data/processedFixData.Rda")

 fixdat$fixX = fixdat$fixX - 512
  
for (s in levels(fixdat$subj))
{
	subjDat = fixdat[which(fixdat$subj==s),]
	subjDat$trialNum = factor(subjDat$trialNum)
	for (t in levels(subjDat$trialNum))
	{
		if (subjDat$easySide[which(subjDat$trialNum==t)][1]=="right")
		{
			idx = which(fixdat$subj==s & fixdat$trialNum==t)
			fixdat$fixX[idx] = - fixdat$fixX[idx]
		}
	}
	rm(subjDat)
}
rm(s,t, idx)

 fixdat$fixX = fixdat$fixX + 512
 fixdat$fixY = fixdat$fixY
# itemdat$itemX = itemdat$itemX + 512 - 64

#
# get saccade info
#
print("...calcualting sacc amp and ang")
fixdat$saccAmp = NaN
fixdat$saccAng = NaN
for (s in levels(fixdat$subj))
{

	subjdat = fixdat[which(fixdat$subj==s),]
	subjdat$trialNum = factor(subjdat$trialNum)
	for (t in levels(subjdat$trialNum))
	{
		if (length(which(fixdat$subj==s & fixdat$trialNum==t))>0)
		{
			saccDat    = saccInfo(fixdat[which(fixdat$subj==s & fixdat$trialNum==t),])		
			fixdat$saccAmp[which(fixdat$subj==s & fixdat$trialNum==t)] = saccDat$amp
			fixdat$saccAng[which(fixdat$subj==s & fixdat$trialNum==t)] = saccDat$ang	
			
			fixdat$fixOn[which(fixdat$subj==s & fixdat$trialNum==t)]=	
			fixdat$fixOn[which(fixdat$subj==s & fixdat$trialNum==t)] - fixdat$fixOn[which(fixdat$subj==s  & fixdat$trialNum==t & fixdat$fixNum==1)]
			rm(saccDat)	
		}
	}

	rm(subjdat)
}
rm(s, t)

#
 dat = fixdat
fixdat = data.frame(subj=dat$subj, version=dat$version, completed=dat$completed, trial=dat$trialNum, targSide=dat$targSideRel, fixNum=dat$fixNum, fixX=dat$fixX, fixY=dat$fixY, fixOn=dat$fixOn, fixDur=dat$fixDur, saccAmp=dat$saccAmp, saccAng=dat$saccAng, easySide=dat$easySide, var=dat$var, condition=dat$condition)



saveRDS(fixdat,file="data/processedFixData.Rda")
write.table(fixdat, "data/processedFixData.txt", sep=",")

#saveRDS(fixdat,file="../data/processedFixData.Rda")
#write.table(fixdat, "../data/processedFixData.txt", sep=",")




