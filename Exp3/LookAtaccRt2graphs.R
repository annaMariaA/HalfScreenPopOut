library(ggplot2)
library(scales)
library(dplyr)

library(lme4)
# setwd("C:/Users/r02al13/Desktop/HalfPopOutAnalysis")
rtdat = readRDS(file="processedRTandAccData.Rda")
# levels(rtdat$targSide) = c("parallel","serial","absent")
cbPalette <- c("#E69F00", "#56B4E9","#B5CB8B")

rtdat$targSide[rtdat$var=="homo" & rtdat$targSide!="absent"] = "homogeneous"

plt = ggplot(rtdat, aes(x=targSide,y=RT, fill=var)) + geom_boxplot()
plt = plt + coord_trans(y="log2") + theme_bw()
plt = plt + scale_y_continuous(name="reaction time (seconds)", breaks=c(1,2,4,8,16,32), limits=c(1,32))
plt
ggsave("../plots/boxplotRT.pdf")
 

m = lmer(log(RT) ~ var + (var|subj), filter(rtdat, targSide=="absent"))

aggDat = (filter(rtdat, targSide=="absent") 
  %>% group_by(subj) 
    %>% summarise(
     hetero = median(RT[var=='hetero'], na.rm=T),
     homo  = median(RT[var=='homo'], na.rm=T),
     split = median(RT[var=='x'], na.rm=T)))

plt = ggplot(aggDat, aes(x=(homo+hetero)/2, y=split)) + geom_point()
plt = plt + geom_abline(slope=1, intercept=0, colour="red")
plt = plt + geom_smooth(method=lm, fullrange=T)
plt = plt + scale_y_continuous(name="TA RT for split stimuli", limits=c(0,15), expand=c(0,0))
plt = plt + scale_x_continuous(name="prediction", limits=c(0,15), expand=c(0,0))
plt = plt + theme_bw()
ggsave("exp3rt.pdf", width=3, height=3)



 library(lme4)
# 
# 
# library(bear)
# library(boot)
# library(gridExtra)
# get_legend<-function(myggplot){
#   tmp <- ggplot_gtable(ggplot_build(myggplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)
# }
# rtdat$subj = factor(as.character(rtdat$subj))
# # let us first look at accuracy for target present and absent
accdat  = aggregate(data=rtdat, acc ~ subj + targSide, FUN="mean")
aggregate(data=rtdat, acc ~ targSide, FUN="mean")

# errorbar <- summarySEwithin(accdat, measurevar="acc", withinvars=c("targSide"), idvar="subj")
# pAcc2 = ggplot(errorbar, aes(x=targSide, y=100*acc, fill=targSide)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
# pAcc2 = pAcc2 + scale_y_continuous(name="Accuracy(%)") + scale_x_discrete(name="Target side")+scale_fill_manual(values=cbPalette)
# pAcc2 = pAcc2 + geom_errorbar(position=position_dodge(.9), within=.25, aes(ymin=(acc-ci)*100,ymax=(acc+ci)*100),width=.3)

# ggsave("../plots/accuracy11.jpg",dpi=600, width=6, height=3)
# write.csv(accdat, "data/accDat.txt", row.names=F)

# # now lets look at RTs... 
# # first we need to filter out incorrect trials
 rtdat = rtdat[which(rtdat$acc==1),]
 library(scales)     
# # rt for target present and absent
 rtdat  = aggregate(data=rtdat, RT ~ subj +targSide, FUN="median")
# #normDataWithin=(data=NULL, idvar, measurevar, betweenvars=NULL,
# errorbar = summarySEwithin(rtdat, measurevar="RT", withinvars=("targSide"), idvar="subj")
# errorbar$lower=c(4.14,1.67, 2.55 )
# errorbar$upper=c(9.87,1.83, 5.33 )

# pRT1 = ggplot(errorbar, aes(x=targSide, y=RT, fill=targSide)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
# pRT1 = pRT1 + scale_y_continuous(name="Reaction time (s)") + scale_x_discrete(name="Target side")+scale_fill_manual(values=cbPalette)
# pRT1 = pRT1 + geom_errorbar(position=position_dodge(.9), within=.25, aes(ymin=lower,ymax=upper),width=.3)

# ggsave("plots/RT.jpg",dpi=600, width=6, height=3)
# write.csv(rtdat, "data/RtData.txt", row.names=F)

# #rtdat3  = aggregate(data=rtdat, RT ~  subj+ hemiType + var + targSide, FUN="median")
# #write.csv(rtdat3, "data/rtData.txt", row.names=F)

# legend<- get_legend(pAcc2)
# legend<- get_legend(pRT1)
# pAcc2<- pAcc2+ theme(legend.position="none")
# pRT1<- pRT1+ theme(legend.position="none")
# jpeg(filename = "../plots/RtAcc1.jpg",width=1200,height=500, pointsize =10, quality = 1000, bg = "white", res = 200, restoreConsole = TRUE)

# grid.arrange(pRT1,pAcc2,ncol=2,widths=c(8,8))
# dev.off()




