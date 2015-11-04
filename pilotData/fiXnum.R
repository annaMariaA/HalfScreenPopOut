setwd("C:/Users/r02al13/Desktop/4ExperiemntsSubmitted/LineSegmEasyDiff")
fixdat = readRDS(file="pilotData/processedFixData.Rda")

fixdat = fixdat[fixdat$targSide=="absent" ,]
fixdat = fixdat[fixdat$hemiType=="Unmodified",]
fixdat = fixdat[fixdat$var=="parallel",]
numFix = aggregate(data=fixdat, fixNum ~ subj+trial, FUN="median")
numFix = aggregate(data=fixdat, fixNum ~ subj, FUN="median")


