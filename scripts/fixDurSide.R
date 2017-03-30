library(dplyr)
library(ggplot2)
library(binom)

# # setwd("~/Desktop/HalfPopOutAnalysisToday")
# setwd("~/Documents/HalfPopOutAnalysisToday")
#<<<<<<< Updated upstream
 # setwd("C:/Users/r02al13/Documents/GitHub/HalfScreenPopOut")
fixdat = readRDS(file="../data/processedFixData.Rda")
fixdat = (filter(fixdat, subj!=4, subj!=15)) 
#=======

fixdat = readRDS(file="../data/processedFixData.Rda")

levels(fixdat$subj) = 1:12
#>>>>>>> Stashed changes
cbPalette <- c("#56B4E9", "#E69F00")
library(lme4)
library(ggplot2)
library(scales)
#library(bear)
#library(boot)

# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "homo"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "hetro"
fixdat$side = as.factor(fixdat$side)


fixdat$saccToSide = NA
for (pp in levels(fixdat$subj))
{
	for (tr in unique(fixdat$trial))
	{
		idx = which(fixdat$subj==pp & fixdat$trial==tr)
		if (length(idx)>1)
		{
			fixdat$saccToSide[idx[1:(length(idx)-1)]] = 
				as.character(fixdat$side[idx[2:length(idx)]])
			}

	}
}
fixdat$saccToSide = as.factor(fixdat$saccToSide)
firstFixDat = filter(fixdat, fixNum==1, saccToSide!="NA")


plt = ggplot(firstFixDat, aes(x=fixDur, fill=saccToSide))
plt = plt + geom_histogram(alpha=0.5)
ggsave("../plots/fixDurFirstFix.pdf")