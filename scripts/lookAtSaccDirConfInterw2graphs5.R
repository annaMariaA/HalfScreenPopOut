library(dplyr)
library(ggplot2)

# # setwd("~/Desktop/HalfPopOutAnalysisToday")
# setwd("~/Documents/HalfPopOutAnalysisToday")

fixdat = readRDS(file="../data/processedFixData.Rda")
cbPalette <- c("#56B4E9", "#E69F00")
library(lme4)
library(ggplot2)
library(scales)
#library(bear)
#library(boot)

nbins = 16
bw = 360/nbins	
# only look at TA trials
fixdat = filter(fixdat, targSide=="absent")

fixdat$hemiType="unmodified"

fixdat$saccAng = (180/pi) * (fixdat$saccAng ) + 180
fixdat$saccAng = ((fixdat$saccAng) %% 360)


# first look at amplitude
ampplot = ggplot(fixdat, aes(x=saccAmp)) + geom_density()
ampplot
saccAngle = data.frame(theta=numeric(), med_amp=numeric(), count=numeric(), hemiType=character())
for (h in levels(fixdat$hemiType))
{
  	# do rest of bins
	for (b in 1:nbins)
    {
    	b1 = (b-1)*bw 
    	b2 = (b)*bw 
 		# get median saccade amplitude for these saccades
   	 	idx = which(fixdat$saccAng>=b1 & fixdat$saccAng<b2 & fixdat$hemiType==h)
   	 	count = length(idx)/length(which(fixdat$hemiType==h))
   	 	medsaccamp = median(fixdat$saccAmp[idx])
  	 	saccAngle = rbind(saccAngle, data.frame(theta=(b-1)*bw+bw/2, med_amp=medsaccamp, count=count, hemiType=h))
   }
}


#rosepltAng <- ggplot(saccAngle, aes(x=theta, y=count)) + geom_bar(width=bw, stat="identity") 
#rosepltAng <- rosepltAng + scale_x_continuous(name=" ", limits=c(0,360), breaks = c(0, 45, 90, 135, 180, 225, 270, 315))
#rosepltAng <- rosepltAng +scale_y_continuous(name=" ",breaks=NULL)+ coord_polar(start=0, direction=1)+theme_bw() + facet_grid(.~hemiType)
#ggsave("../plots/roseplot.pdf", width=10, height=5)



#rosepltAng <- ggplot(saccAngle, aes(x=theta, y=med_amp)) + geom_bar(width=bw, stat="identity") 
#rosepltAng <- rosepltAng + scale_x_continuous(name=" ", limits=c(0,360), breaks = c(0, 45, 90, 135, 180, 225, 270, 315))
#rosepltAng <- rosepltAng +scale_y_continuous(name=" ",breaks=NULL)+ coord_polar(start=0, direction=1)+theme_bw() + facet_grid(.~hemiType)



# Plot distribution of fixation x-coord by fixation number

m = ggplot(
	data=filter(fixdat, fixNum<13, fixX<1024, fixX>0), 
	aes(x = fixX))
m = m + geom_histogram(fill="purple", binwidth=64) 
m = m + facet_wrap(~fixNum, scales="free_y", nrow=4)
m = m + theme_bw()
m = m + scale_x_continuous(name="fixation horizontal postition", breaks=c(0,512,1024), expand=c(0,0))
ggsave("../plots/FixXpostByFixNum.pdf", width=9, height=9)


# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "homo"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "hetro"
fixdat$side = as.factor(fixdat$side)

pltDat = aggregate(data=fixdat, fixX ~ side + fixNum, FUN="length")
pltDat = filter(pltDat, fixNum<5)
plt = ggplot(pltDat, aes(x=side, y=fixX, fill=side))  + geom_bar(stat="identity")
plt = plt + facet_wrap(~fixNum)
ggsave("../plots/FixXsideByFixNum.pdf", width=9, height=9)

# now exlcude proportion 
propDat = filter(fixdat, side!="central", fixNum<11)
propDat$propHetro = (propDat$side == "hetro")
propDat = aggregate(data=propDat, propHetro~subj + fixNum, FUN="mean")

plt = ggplot(propDat, aes(x=fixNum, y=propHetro, colour=subj))
plt = plt + geom_point() + geom_smooth(se=F)
plt = plt + theme_bw()
ggsave("../plots/FixXsideByFixNumAndSubj.pdf", width=9, height=9)