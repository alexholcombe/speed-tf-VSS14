#Intended to be called by doAllAnalyses.R, which then
#assumed global variables: fitParms, psychometrics, and function calcPctCorrThisSpeed
#assumed global variables: threshes, critCriteria

#Just look at critical threshes that correct for chance across numObjects conditions
#intersect critCriteria with threshes
thrCrit<-merge(critCriteria,threshes,by=c("numObjects","criterion"),all=FALSE) 
#LN numObjects=2, numTargets=1  she doesn't go low enough. But fit should, so need to go to faster speeds
#tmp <- subset(psychometrics,subject=="LN" & numObjects==2 & numTargets==1)

##########Plot individual data points for each subject. Pattern remarkably consistent across Ss, perhaps show in paper?
quartz(title="P individual Ss threshes",width=8,height=6) #create graph of thresholds
h<-ggplot(data=thrCrit,aes(x=numTargets,y=thresh,color=factor(numObjects)))
#h<-h+facet_grid(criterion ~ exp)+theme_bw()
h<-h+theme_bw() 
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
show(h)
############################ 
tit<-paste('threshes',max(critCriteria$criterion)*100,'andScaled*',iv,sep='' )
quartz(title=tit,width=5,height=4) #create graph of thresholds
thrCrit$numObjects <- as.numeric(thrCrit$numObjects) #Otherwise can't connect with lines
thrCrit$targets <- thrCrit$numTargets #Otherwise can't connect with lines
#h<-ggplot(data=thrCrit,aes(x=targets,y=thresh,color=factor(numObjects)))
h<-ggplot(data=thrCrit,aes(x=numObjects-1,y=thresh,linetype=factor(numRings),color=targets))
h<-h+theme_bw() 
if (iv=="speed") { h=h+ylab('Speed (rps)') } 
if (iv=="tf") { h=h+ylab('Temporal frequency (Hz)') } 
h<-h + xlab('Number of distractors')

#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
dodgeWidth<-.15
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth)) 
#h<-h+ stat_summary(fun.y=mean,geom="line",size=1)  #I don't understand why line doesn't work
h<-h+ stat_summary(fun.ymax= errBarUpper, fun.ymin= errBarLower, geom = "errorbar",width=.5,position=position_dodge(width=dodgeWidth)) 
#h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.95) #error bar
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
h<-h+ theme_bw()+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
if (!varyLapseRate)
  h<-h+ggtitle('lapse rate always zero')
show(h)
ggsave( paste('figs/',tit,'.png',sep=''), dpi=300  )

#Also need to do it for a single criterion, to make sure how control for number of objects doesn't matter
thr<-subset(threshes,criterion==0.80)
thr$numObjects <- as.numeric(thr$numObjects) #Otherwise can't connect with lines
thr$targets <- thr$numTargets #Otherwise can't connect with lines
tit<-paste('threshes0.8pct*',iv,sep='' )
quartz(title=tit,width=5,height=4) #create graph of thresholds
#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
h<-ggplot(data=thr,aes(x=numObjects-1,y=thresh,linetype=factor(numRings),color=targets))
h<-h + xlab('Number of distractors')
h<-h+theme_bw() 
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth)) 
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
h<-h+ stat_summary(fun.ymax= errBarUpper, fun.ymin= errBarLower, geom = "errorbar",width=.5,position=position_dodge(width=dodgeWidth)) 
#h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.95) #error bar
h<-h+ theme_bw()+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
if (!varyLapseRate)
  h<-h+ggtitle('lapse rate always zero')
show(h)
ggsave( paste('figs/',tit,'.png',sep=''), dpi=300  )

############################################################################################################
#Temporal frequency plot CHANGE THIS TO CONVERT TO OPPOSITE OF IV
#BUT PROBABLY SUPERFLUOUS NOW THAT I'M DOING IT THE RIGHT WAY
# thrCrit$temporalFreq <- thrCrit$thresh*thrCrit$numObjects
# ##########Plot individual data points for each subject. Pattern remarkably consistent across Ss, perhaps show in paper?
# quartz(title="P individual Ss temporal freq",width=8,height=6) #create graph of thresholds
# h<-ggplot(data=thrCrit,aes(x=numTargets,y=temporalFreq,color=factor(numObjects)))
# #h<-h+facet_grid(criterion ~ exp)+theme_bw()
# h<-h+theme_bw() 
# #ylim(1.4,2.5) DO NOT use this command, it will drop some data
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
# h<-h+ylab('Temporal frequency thresh')
# show(h)
# ##########Temporal freq mean across Ss
# tit<-paste("TemporalFreqCritThresh",sep='' ) 
# quartz(title=tit,width=5,height=4) #create graph of thresholds
# #Make the variable names nicer for the plot
# thrCrit$numObjects <- as.numeric(thrCrit$numObjects) #Otherwise can't connect with lines
# thrCrit$targets <- thrCrit$numTargets #Otherwise can't connect with lines
# h<-ggplot(data=thrCrit,aes(x=numObjects-1,y=temporalFreq,color=targets))
# h<-h+theme_bw() 
# h<-h+ ylab('Temporal frequency threshold (Hz)') + xlab('Number of distractors')
# 
# #ylim(1.4,2.5) DO NOT use this command, it will drop some data
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# #h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
# dodgeWidth<-.12
# h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth)) 
# #h<-h+ stat_summary(fun.y=mean,geom="line",size=1)  #I don't understand why line doesn't work
# h<-h+ stat_summary(fun.ymax= errBarUpper, fun.ymin= errBarLower, geom = "errorbar",width=.5,position=position_dodge(width=dodgeWidth)) 
# #h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.95) #error bar
# h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
# h<-h+ theme_bw()+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
# 
# #h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
# if (!varyLapseRate)
#   h<-h+ggtitle('lapse rate always zero')
# show(h)
