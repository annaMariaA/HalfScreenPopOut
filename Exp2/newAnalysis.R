# look at the residual from the simple (easy-hard)/2 model 
library(tidyverse)
library(scales)
library(binom)
library(ggthemes)
library(lme4)

rtdat = readRDS(file="processedRTandAccData.Rda")
fixdat = readRDS(file="processedFixData.Rda")



rtdat$targSide[rtdat$var=="homo" & rtdat$targSide!="absent"] = "homogeneous"


# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "homo"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "hetro"
fixdat$side = as.factor(fixdat$side)

stratDat = (filter(fixdat, 
	side!="central", 
	fixNum<6, fixNum>1, 
	targSide == "absent",
	condition == "half") 
  %>% group_by(subj) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))



 

m = lmer(log(RT) ~ var + (var|subj), filter(rtdat, targSide=="absent"))

aggDat = (filter(rtdat, targSide=="absent", acc == 1) 
  %>% group_by(subj) 
    %>% summarise(
     hetero = median(RT[var=='hetero'], na.rm=T),
     homo  = median(RT[var=='homo'], na.rm=T),
     split = median(RT[var=='x'], na.rm=T)))


aggDat$res = with(aggDat, split - (hetero + homo)/2)


dat <- full_join(aggDat, stratDat)

r <- cor.test(dat$res, dat$propHetro)$estimate
r <- round(r, 2)

plt <- ggplot(dat, aes(x = res, y = propHetro))
plt <- plt + geom_point()
plt <- plt + geom_smooth(method = "lm", colour = "firebrick4")
plt <- plt + scale_x_continuous("rt - (hard + easy)/2 (secs)")
plt <- plt + scale_y_continuous("prop. fixations to hetero. side", limits = c(0, 1))
plt <- plt + theme_solarized()
plt <- plt + geom_text(label = paste("r = ", r, sep = " "), x = 1, y= 0.4)
ggsave("residual_strat_corr.pdf", width = 3, height = 3)


r <- cor.test(dat$split, dat$propHetro)$estimate
r <- round(r, 2)

plt <- ggplot(dat, aes(x = split, y = propHetro))
plt <- plt + geom_point()
plt <- plt + geom_smooth(method = "lm", colour = "firebrick4")
plt <- plt + scale_x_continuous("split half rt (secs)")
plt <- plt + scale_y_continuous("prop. fixations to hetero. side", limits = c(0, 1))
plt <- plt + theme_solarized()
plt <- plt + geom_text(label = paste("r = ", r, sep = " "), x = 5, y= 0.4)
ggsave("rt_strat_corr.pdf", width = 3, height = 3)


# r <- cor.test(dat$split, dat$propHetro)$estimate
# r <- round(r, 2)

# plt <- ggplot(dat, aes(x = split, y = res))
# plt <- plt + geom_point()
# plt <- plt + geom_smooth(method = "lm", colour = "firebrick4")
# plt <- plt + scale_x_continuous("split half rt (secs)")
# plt <- plt + scale_y_continuous("rt - (hard + easy)/2 (secs)")
# plt <- plt + theme_solarized()
# plt <- plt + geom_text(label = paste("r = ", r, sep = " "), x = 5, y= 0.4)
# ggsave("rt_residual.pdf", width = 3, height = 3)


#  now do simple model
m <- lm(
	data = dat,
	propHetro ~ split * res)

dat$p <- predict(m)

r <- cor.test(dat$p, dat$propHetro)$estimate
r <- round(r, 2)

plt <- ggplot(dat, aes(x = p, y = propHetro))
plt <- plt + geom_point()
plt <- plt + geom_smooth(method = "lm", colour = "firebrick4")
plt <- plt + scale_x_continuous("lm prediciton", limits = c(0, 1))
plt <- plt + scale_y_continuous("prop. fixations to hetero. side", limits = c(0, 1))
plt <- plt + theme_solarized()
plt <- plt + geom_text(label = paste("r = ", r, sep = " "), x = 0.7, y= 0.25)
ggsave("lm_strat_corr.pdf", width = 3, height = 3)



