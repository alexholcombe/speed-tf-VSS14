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
myPlotCurve <- makeMyPlotCurve4("speed",0.03,8,numPointsForPsychometricCurve)
#mean, slope, lapseRate, chanceRate, method, linkFx
dd<- subset(thrAll,(exp=="4a"|exp=="4b") & iv=="speed" & criterionNote=="threeQuarters")
dd<-subset(dd,numTargets==1)  
#Find average params for each numObjects condition
modeOfList<- function(l) { names(which.max(table(l)))[1] } #R doesn't have built-in mode function!
avg<- dplyr::summarise( dplyr::group_by(dd, numObjects),   mean=mean(mean), numTargets=1,
          thresh=mean(thresh),slope=mean(slope),lapseRate=mean(lapseRate),chanceRate=mean(chanceRate),
          method=modeOfList(method), linkFx=modeOfList(linkFx), iv="speed" )
#tf<-subset(thrAll,(exp=="4a") & iv=="tf" & numTargets==1 & numObjects==9) #Used for VSS2014 poster. Want to compare to updated
avgTf<- dplyr::summarise( dplyr::group_by(tf, numObjects),   mean=mean(mean), numTargets=1,
                        thresh=mean(thresh),slope=mean(slope),lapseRate=mean(lapseRate),chanceRate=mean(chanceRate),
                        method=modeOfList(method), linkFx=modeOfList(linkFx), iv="tf" )
avg<-rbind(avg,avgTf)
#Plug in average params to get average psychometric functions
avgPsycho<-ddply(avg,.(numObjects,iv),myPlotCurve)
#Plot speed limits. Compre to old VSS2014 poster
#Create psychometric curve predicted by tf limit for 2-object condition. Take 9-object psychometric function, 
#speed=tf/numObjects
#9 object case has speeds tf/9. We want speeds of tf/2. So, multiply speeds by 9/2
tfLimFor2objs<- subset(avgPsycho,numObjects==9 & iv=="speed")
tfLimFor2objs$speed<- tfLimFor2objs$speed*9/2
tfLimFor2objsVSS14poster<- subset(avgPsycho,numObjects==9 & iv=="tf")
tfLimFor2objsVSS14poster$speed<- tfLimFor2objsVSS14poster$speed/2 #"speed" is actually tf
#The only problem is that chance will now be 50%. So simply rescale to that?
#Rescale to 50% to 1. Although need to take into account guess rate.
l=0.01 #lapse rate
tfLimFor2objs$correct<- (tfLimFor2objs$correct-1/9)/(1-l-1/9) * (0.5-l) + 0.5
tfLimFor2objsVSS14poster$correct<- (tfLimFor2objsVSS14poster$correct-1/9)/(1-l-1/9) * (0.5-l) + 0.5
tfLimFor2objs$type<-"tf"
tfLimFor2objsVSS14poster$type<-"tf"

twoObjs<-subset(avgPsycho,numObjects==2)
twoObjs$type<-"actual"
twoObjs<-rbind(twoObjs,tfLimFor2objs)
twoObjsWithVSS14<-rbind(twoObjs,tfLimFor2objsVSS14poster)
#g<-ggplot(avgPsycho,aes(x=speed,y=correct,color=factor(numObjects)))
g<-ggplot(twoObjsWithVSS14,aes(x=speed,y=correct,color=factor(type),shape=factor(iv)))
g<-g+geom_point()
g<-g+coord_cartesian(xlim=c(0,4))
g<-g+geom_hline(yintercept=0.88)
g #look nearly identical, the e4a tf fit function and that of the e4a,e4b average speed fit
#Now I see why poster threshold difference was 0.8 rps, whereas I've got 0.5 rps-
#It's because the 75% threshold difference is indeed 0.8 rps different.
#Now, recreate the rest of the VSS poster graphs.
tit<-'Example_actual_and_Hz_limit'
quartz(tit,width=4,height=4)
g<-ggplot(twoObjs,aes(x=speed,y=correct,color=factor(type)))
g<-g+geom_line()
g<-g+coord_cartesian(xlim=c(0,4))
g<-g+geom_hline(yintercept=0.88,linetype=2)
g
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
##########################################
#Show tf limit predicted for each number of objects
tfLim<- subset(avgPsycho,numObjects==9 & iv=="speed")
#replicate it for each numObjects condition that want to simulate
objConds<- unique(avgPsycho$numObjects)
numObjConds<- length( objConds )
tfLimPredictd<- tfLim[ rep(seq(nrow(tfLim)),numObjConds) , ]
#Create the numObjects to go with it
times<- (nrow(tfLimPredictd)) / numObjConds
numObj<- rep(objConds, each=times)
tfLimPredictd$numObjects<-numObj
tfLimPredictd$speed<- tfLimPredictd$speed*9/tfLimPredictd$numObjects
tfLimPredictd$chance<- 1/tfLimPredictd$numObjects
tfLimPredictd$correct<- (tfLimPredictd$correct-1/9)/(1-l-1/9) *
                         (1-l-tfLimPredictd$chance) + tfLimPredictd$chance

tfLimPredictd$type="tf"
observd<-avgPsycho
observd$chance<- 1/observd$numObjects
observd$type<- "observed"
both<- rbind(tfLimPredictd,observd)
both<- subset(both, numObjects!=9) #because prediction and observed identical there.
both$objects<- paste0(both$numObjects," objects") #text for different plot panels
#Set threshold criterion depending on numObjects
both$criterion<- 1/both$numObjects + 0.75*(1-1/both$numObjects)
both$criterion<- round(both$criterion,3)
both$chance<- 1/both$numObjects
tit<-'Rps_and_Hz_limits_each_numObjs'
quartz(tit,width=4,height=7)
g<-ggplot(both,aes(x=speed,y=correct,color=type))
g<-g+facet_grid(objects~.)
g<-g+geom_line()
g<-g+xlab("speed (rps")

criteria<- 1/unique(both$numObjects) + 0.75*(1-1/unique(both$numObjects))
g<-g+geom_hline(aes(yintercept=chance),linetype=2, color="grey")
g<-g+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g<-g+theme(         strip.background = element_rect(fill = 'transparent',color='white') )
g<-g+scale_color_manual(values=c("black","red","blue"))
#use point by point search to find the threshold. 
threeQuarterThreshExtract<- function(psychometric) {
  numObjectsThis<- psychometric$numObjects[1]
  #calculate three-quarters thresh
  myThreshGetNumeric<- makeMyThreshGetNumerically("speed",psychometric$criterion[1])
  threshesThisNumeric<- myThreshGetNumeric(psychometric)
  threshesThisNumeric$criterion<- psychometric$criterion[1]
  return (threshesThisNumeric)
}
threshesThisNumeric = ddply(both,.(numObjects,type),threeQuarterThreshExtract)
#Create threshold line
threshLine <- function(df) {   
  #assumes that df has column "thresh", the threshold value, also must have "criterion"
  threshes = df$thresh
  speeds=c(threshes[1],threshes[1])
  yMin=0
  corrects=c(df$criterion,yMin-.2) #draw down to horizontal axis. The -.2 makes sure it extends into margin
  grid<-data.frame(speed=speeds,correct=corrects)
  return (grid) 
}
threshLines <- ddply(threshesThisNumeric,.(numObjects,type),threshLine)
threshLines$objects<- paste0(threshLines$numObjects," objects") #text for different plot panels
g<-g+ geom_line(data=threshLines,lty=2)  #,color="black") #emphasize lines so can see what's going on
g<-g+coord_cartesian(xlim=c(0.3,3.5),ylim=c(0.1,1))
g
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png

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