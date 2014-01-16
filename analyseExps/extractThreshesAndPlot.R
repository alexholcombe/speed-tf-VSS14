#Intended to be called by doAllAnalyses.R, which 
#provides fitParms, psychometrics, and function calcPctCorrThisSpeed

#go point by point to find thresholds for each criterion
#worstLapseRate <- max(fitParms$lapseRate)
#paste("Can't calculate threshold above criterion level of",1-worstLapseRate,"because that's the worst subject")
#maxCriterion <- 1-worstLapseRate
maxCriterion <- .95
#seq(from=0.67,to=maxCriterion,by=0.03) 
#threshCriteria<- (1.00 + 1 / unique(fitParms$numObjects)) / 2.0  #midpoint thresholds
threshes <- data.frame()
#psychometrics <- thisPsychometrics
for (numObjectsThis in unique(fitParms$numObjects)) {
  #for (threshCriterion in threshCriteria) {
  threshCriterion <- (1.00 + 1/numObjectsThis) / 2.0  #midpoint threshold
  
  cat('Testing criterion:',threshCriterion)
  #use point by point search to find the threshold. 
  myThreshGetNumeric= makeMyThreshGetNumerically(threshCriterion)
  
  psychometricTemp<- subset(psychometrics,numObjects==numObjectsThis)
  calcThreshForPredictn<- FALSE  #because tracking-two prediction for 6, 9 objects never gets that high. Anyway this is to address
  if (!calcThreshForPredictn)  
    psychometricTemp <- subset(psychometricTemp,numTargets!="2P")
  #Don't do it where criterion corresponds to below chance
  #psychometricTemp <- subset(psychometricTemp, numObjects > 1/threshCriterion) #For these numObjects conditions, chance is above the current criterion
  
  threshesThisNumeric = ddply(psychometricTemp,factorsPlusSubject,myThreshGetNumeric) 
  threshesThisNumeric$criterion <- threshCriterion
  threshesThis<- merge(threshesThisNumeric,fitParms)
  threshes<- rbind(threshes, threshesThis)
}

#LN numObjects=2, numTargets=1  she doesn't go low enough. But fit should, so need to go to faster speeds
#tmp <- subset(psychometrics,subject=="LN" & numObjects==2 & numTargets==1)

##########Plot individual data points for each subject. Pattern remarkably consistent across Ss, perhaps show in paper?
tit="individual Ss threshesSpeed"
quartz(title=tit,width=4,height=3) #create graph of thresholds
h<-ggplot(data=threshes,aes(x=numTargets,y=thresh,color=factor(numObjects)))
#h<-h+facet_grid(criterion ~ exp)+theme_bw()
h<-h+theme_bw() 
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
h<-h+ylab('thresh (rps)')
h<-h+ggtitle("6,9 difft validates t.f. limit. Speed limits vary widely")
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
############################
tit<-"threshesSpeed"
quartz(title=tit,width=4,height=3) #create graph of thresholds
h<-ggplot(data=threshes,aes(x=numTargets,y=thresh,color=factor(numObjects)))
#h<-h+facet_grid(criterion ~ exp)+theme_bw()
h<-h+theme_bw() 
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
h<-h+ stat_summary(fun.y=mean,geom="point")
#h<-h+ stat_summary(fun.y=mean,geom="line",size=1)  #I don't understand why line doesn't work
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.95) #error bar
h<-h+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
h<-h+ylab('thresh (rps)')
h<-h+ggtitle("6,9 difft validates t.f. limit. Speed limits vary widely")
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
if (!varyLapseRate)
  h<-h+ggtitle('lapse rate always zero')
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )

############################################################################################################
#Temporal frequency plot
threshes$temporalFreq <- threshes$thresh*threshes$numObjects
##########Plot individual data points for each subject. Pattern remarkably consistent across Ss, perhaps show in paper?
tit="individualSsTemporalFreq"
quartz(title=tit,width=5,height=3.5) #create graph of thresholds
h<-ggplot(data=threshes,aes(x=numTargets,y=temporalFreq,color=factor(numObjects)))
#h<-h+facet_grid(criterion ~ exp)+theme_bw()
h<-h+theme_bw() 
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
h<-h+ylab('thresh (Hz)')
h<-h+ggtitle("6,9 on top each other, validating t.f. limit")
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
##########################################
tit="temporalFreqMeanThreshes"
quartz(title=tit,width=4,height=3) #create graph of thresholds
#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
h<-ggplot(data=threshes,aes(x=numTargets,y=temporalFreq,color=factor(numObjects)))
#h<-h+facet_grid(criterion ~ exp)+theme_bw()
h<-h+theme_bw() +ylab('thresh (Hz)')
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
dodgeAmt=.3
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeAmt))
#h<-h+ stat_summary(fun.y=mean,geom="line",size=1)  #I don't understand why line doesn't work
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeAmt)) 
h<-h+ theme_bw()+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
if (!varyLapseRate)
  h<-h+ggtitle('lapse rate always zero')
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )

#MAKE SURE I got the slopes. I'm still getting insanely large slopes. Can I fit with something else?!
#WYC 2 targets 2 objects has particularly high slopes
###########################################################
#Disprove Franconeri by making the theory he's talking about. But that also seems to require reasonable slopes

cat('I give you threshes')
###################################
#plot thresholds (at only one criterion level) for all 3 experiments at same time
# quartz()
# #tt<-subset(threshes,subject=="AH");  tt<-subset(tt,numTargets=="1")
# #tt$subject<-factor(tt$subject) #in case unused levels were the problem
# #h<-ggplot(data= fake, aes(x=separatnDeg,y=thresh))
# h<-ggplot(data= subset(threshes,numTargets!="2P"), aes(x=separatnDeg,y=thresh,color=numTargets,shape=exp))
# h<-h + facet_grid(exp~., scales="free") # ~criterion
# #h<-h+stat_summary(data=threshesThisNumeric,fun.data="mean_cl_boot",geom="errorbar",conf.int=.95,position=position_dodge(width=.2)) #error bar
# h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",conf.int=.95,position=position_dodge(width=.2)) #error bar
# h<-h+theme_bw() + xlab("Separation (deg)")
# #ylim(1.4,2.5) DO NOT use this command, it will drop some data
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# #h<-h+coord_cartesian(ylim=c(1.4,2.6)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
# h<-h+ stat_summary(fun.y=mean,geom="point") + stat_summary(fun.y=mean,geom="line") 
# h+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines
# h<-h+ggtitle(paste('lapse rate',lapseMinMax))
# h
