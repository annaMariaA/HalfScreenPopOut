#setwd("C:/Users/r02al13/Desktop/4ExperiemntsSubmitted/LineSegmEasyDiff")
fixdat = readRDS(file="processedFixData.Rda")

#mean number of fixations on the target absent trials/parallel full screen
fixdatFull = fixdat[fixdat$var=="homo" ,]
fixdatFull = fixdat[fixdatFull$targSide=="absent" ,]
fixdatFull = fixdat[fixdatFull$condition=="full",]

numFix = aggregate(data=fixdatFull, fixNum ~ subj+trial, FUN="max")
numFix = aggregate(data=numFix, fixNum ~ subj, FUN="median")
mean(numFix$fixNum)

#mean number of fixations on the target absent trials/parallel half of the screen
# classify every fixation as homo (left), central, or hetro (right)
fixdatHalf = fixdat[fixdat$targSide=="absent" ,]
fixdatHalf = fixdatHalf[fixdatHalf$condition=="half",]
centralWidth = 64 #change to 1 visual degree
fixdatHalf$side = 'central'
fixdatHalf$side[which(fixdatHalf$fixX <(512-centralWidth/2))] = "homo"
fixdatHalf$side[which(fixdatHalf$fixX >(512+centralWidth/2))] = "hetro"
fixdatHalf$side = as.factor(fixdatHalf$side)
fixdatHalf = fixdatHalf[fixdatHalf$side=="homo" ,]

numFixHalf = aggregate(data=fixdatHalf, fixNum ~ subj+trial, FUN="length")
numFixHalf = aggregate(data=numFixHalf, fixNum ~ subj, FUN="median")
mean(numFixHalf$fixNum)