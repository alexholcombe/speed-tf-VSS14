#To be called by doAllAnalyses_E4ab.R, thus assumes it is in that, parent, directory

#Assumes 'threshes_tf_postVSS_13targets2349objects.Rdata' and 'threshes_speed_postVSS_13targets2349objects'
#created by doAllAnalyses_E4ab.R
load("../data/thrAll.Rdata",verbose=TRUE)

#Fittng psychometric functions with penalized likelihood prevented extremely large slopes
numExtremeSlopes<- sum(thrAll$slopeThisCrit < -10) #not a number
if (is.na(numExtremeSlopes)) numExtremeSlopes<-0
if (numExtremeSlopes) {
  writeLines( paste("Your psychometric function fitting yielded",numExtremeSlopes," extremely steep slopes"))
}
###Create mean psychometric function, to show lack of overlap between tf limit
#and speed limit for objects=2
source('helpers/psychometricHelpRobust6.R') #for makeMyPlotCurve4
numPointsForPsychometricCurve=500
myPlotCurve <- makeMyPlotCurve4("speed",0.03,4,numPointsForPsychometricCurve)
#mean, slope, lapseRate, chanceRate, method, linkFx
dd<- subset(thrAll,(exp=="4a"|exp=="4b") & iv=="speed" & criterionNote=="threeQuarters")
dd<-subset(dd,numTargets==1)  
#Find average params for each numObjects condition
modeOfList<- function(l) { names(which.max(table(l)))[1] } #R doesn't have built-in mode function!
avg<- dplyr::summarise( dplyr::group_by(dd, numObjects),   mean=mean(mean), numTargets=1,
          thresh=mean(thresh),slope=mean(slope),lapseRate=mean(lapseRate),chanceRate=mean(chanceRate),
          method=modeOfList(method), linkFx=modeOfList(linkFx) )
#Plug in average params to get average psychometric functions
avgPsycho<-ddply(avg,"numObjects",myPlotCurve)
#Plot speed limits.
#Create psychometric curve predicted by tf limit for 2-object condition. Take 9-object psychometric function, 
#speed=tf/numObjects
#9 object case has speeds tf/9. We want speeds of tf/2. So, multiply speeds by 9/2
tfLimFor2objs<- subset(avgPsycho,numObjects==9)
tfLimFor2objs$speed<- tfLimFor2objs$speed*9/2
#The only problem is that chance will now be 50%. So simply rescale to that?
#Rescale to 50% to 1. Although need to take into account guess rate.
l=0.01 #lapse rate

tfLimFor2objs$correct<- (tfLimFor2objs$correct-1/9)/(1-1/9) * 0.5 + 0.5
tfLimFor2objs$type<-"tf"
twoObjs<-subset(avgPsycho,numObjects==2)
twoObjs$type<-"actual"
twoObjs<-rbind(twoObjs,tfLimFor2objs)

g<-ggplot(avgPsycho,aes(x=speed,y=correct,color=factor(numObjects)))
g<-ggplot(twoObjs,aes(x=speed,y=correct,color=factor(type)))
g<-g+geom_point()
g<-g+coord_cartesian(xlim=c(0,4))
g<-g+geom_hline(yintercept=0.88)
g
##########################################tf mean slope against targets
tit=paste0(expNames,"_tfSpeedSlopeMeanAgainstDistractors_",infoMsg,"_threeQuarterThresh")
quartz(title=tit,width=6,height=3)
#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
d<-subset(thr,criterionNote=="threeQuarters")
d$targets<-as.factor(d$numTargets); d$objects<-d$numObjects
h<-ggplot(data=d,aes(x=objects,y=-slopeThisCrit,color=targets))
h<-h+facet_grid(iv ~ exp, scales="free_y")
#h<-h+facet_grid(exp ~ iv, scales="free_y")
dodgeAmt<-.6
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeAmt))
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeAmt))
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeAmt)) 
h<-h+ylab('slope')
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
h<-h+scale_x_continuous(breaks=c(3,6,9,12))
h<-h+ggtitle(paste(tit,lapseMsg))
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
#None of these make the slopes particularly flat.
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png

#################SLOPES##################
# h<-ggplot(data=threshes,aes(x=separatnDeg,y=-slopeThisCrit,color=numTargets))
# 
# tit=paste0(expNames,"_tfMeanThreshAgainstDistractors ",infoMsg," threeQuarterThresh")
# quartz(title=tit,width=6,height=3)
# #Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
# d<-subset(threshes,criterionNote=="threeQuarters")
# d$targets<-as.factor(d$numTargets); d$objects<-d$numObjects
# h<-ggplot(data=d, aes(x=objects,y=tfThresh,color=targets))
# h<-h+facet_grid(exp ~ .)  #facet_grid(criterion ~ exp)
# h<-h+ylab('threshold (Hz)')
# h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# h<-h+scale_x_continuous(breaks=c(3,6,9,12))
# dodgeAmt=.25
# h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeAmt))
# h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeAmt))
# h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeAmt)) 
# h<-h+ggtitle(paste(tit,lapseMsg))
# show(h) #http://stack