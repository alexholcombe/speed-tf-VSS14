#Intended to be called by doAllAnalyses_e4ab.R, which 
#variables expected:
#thr containing thresholds
infoMsg=paste0(iv,"-fit")

lapseMsg=""
if (!varyLapseRate)
  lapseMsg=paste("lapseRate always",unique(lapseMinMax))

themeAxisTitleSpaceNoGridLinesLegendBox = theme_classic() + #Remove gridlines, show only axes, not plot enclosing lines
  theme(axis.line = element_line(size=.3, color = "grey"), 
        axis.title.y=element_text(vjust=0.24), #Move y axis label slightly away from axis
        axis.title.x=element_text(vjust=.10), #Move x axis label slightly away from axis
        legend.key = element_blank(), #don't put boxes around legend bits
        legend.background= element_rect(fill="transparent",color="grey90"), #put big light grey box around entire legend
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.background = element_rect(fill = 'transparent',color='white')
        #strip.text.y= element_text(vjust=0, size=14)  #seems to have no effect
          )
#############################################Plot mean tf,speed threshes against distractors
tit<-paste0(expNames,'_MeanAgainstDistractors ',infoMsg,'_threeQuarterThresh') 
quartz(title=tit,width=6,height=3) #create graph of threshes
thr$objects <- as.numeric(thr$numObjects) #Otherwise can't connect with lines
thr$targets <- as.factor(thr$numTargets) 
d<-subset(thr,criterionNote=="threeQuarters") # & exp!="HC2013")
h<-ggplot(data=d, aes(x=objects,y=thresh,color=targets)) 
h<-h+facet_grid(iv ~ exp, scales="free_y")  
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
h<-h+scale_y_continuous(breaks=seq(0,6)) #No way to set axis labels independently. Could be clever with a custom axis labeller ?scales::trans_new
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
dodgeWidth<-.25
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth))
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeWidth)) 
h<-h+ylab('tf (Hz)                  speed (rps)')
h<-h+ggtitle(paste("5,8 difft validates t.f. limit. Speed limits vary widely",lapseMsg))
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
#############################################Plot mean speed threshes against distractors
tit<-paste0(expNames,'_SpeedMeanThreshAgainstDistractors ',infoMsg,'_threeQuarterThresh') 
quartz(title=tit,width=6,height=3) #create graph of threshes
threshes$objects <- as.numeric(threshes$numObjects) #Otherwise can't connect with lines
threshes$targets <- as.factor(threshes$numTargets) 
t<-subset(threshes,criterionNote=="threeQuarters") # & exp!="HC2013")
h<-ggplot(data=t, aes(x=objects,y=thresh,color=targets)) #I have no idea why but this doesn't work, hence put it in facet_grid
h<-h+facet_grid(. ~ exp)  #facet_grid(criterion ~ exp)
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
dodgeWidth<-.25
h<-h+scale_x_continuous(breaks=c(3,6,9,12))
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth))
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeWidth)) 
h<-h+ylab(  paste('threshold', ' (',ifelse(dv=="speed","rps","Hz"),')',sep='')  )
h<-h+ggtitle(paste("5,8 difft validates t.f. limit. Speed limits vary widely",lapseMsg))
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
##########################################
tit=paste0(expNames,"_tfMeanThreshAgainstDistractors ",infoMsg," threeQuarterThresh")
quartz(title=tit,width=6,height=3)
#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
d<-subset(threshes,criterionNote=="threeQuarters")
d$targets<-as.factor(d$numTargets); d$objects<-d$numObjects
h<-ggplot(data=d, aes(x=objects,y=tfThresh,color=targets))
h<-h+facet_grid(exp ~ .)  #facet_grid(criterion ~ exp)
h<-h+ylab('threshold (Hz)')
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
h<-h+scale_x_continuous(breaks=c(3,6,9,12))
dodgeAmt=.25
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeAmt))
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeAmt))
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeAmt)) 
h<-h+ggtitle(paste(tit,lapseMsg))
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
##########Plot threshes, exp*subject*numTargets*numObjects ################
expNames<- paste0( unique(threshes$exp), collapse="" )
tit=paste(expNames,"_indivSs_threshesSpeed_",infoMsg,"_threeQuarterThresh",sep='')
dv="speed"
quartz(title=tit,width=6,height=3) #create graph of thresholds
h<-ggplot(data=subset(threshes,criterionNote=="threeQuarters"),   #midpoint
          aes(x=targets,y=thresh,color=distractors))
h<-h+facet_grid(. ~ exp)  #facet_grid(criterion ~ exp)
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
xTicks= unique(threshes$targets) #put axis ticks at actual values used
h<-h+scale_x_continuous(breaks=c( xTicks ))
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
h<-h+ylab(  paste('threshold ',iv,' (',ifelse(dv=="speed","rps","Hz"),')',sep='') )  
if (iv=="speed") { h<-h+ggtitle("Speed limits vary widely. 6,9 will converge when plot tf") 
} else h<-h+ggtitle('6,9 validate tf limit.')
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
#############################################Plot mean speed threshes against numTargets
tit<-paste0(expNames,"_SpeedMeanThreshAgainstTargets_",infoMsg,"_threeQuarterThresh")
quartz(title=tit,width=4,height=3) 
#g<-g+guides(color=guide_legend(title="targets")) #change legend title
h<-ggplot(data=subset(threshes,criterionNote=="threeQuarters"),   
          aes(x=numTargets,y=thresh,color=objects))
h<-h+facet_grid(. ~ exp)  #facet_grid(criterion ~ exp)
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
h<-h+ stat_summary(fun.y=mean,geom="point")
h<-h+ stat_summary(fun.y=mean,geom="line")
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.95) #error bar
h<-h+ylab(  paste('threshold ',iv,' (',ifelse(iv=="speed","rps","Hz"),')',sep='')  ) 
if (iv=="speed") {  h<-h+ggtitle("6,9 difft validates t.f. limit. Speed limits vary widely")
} else h<-h+ggtitle('6,9 validate tf limit.')
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
h<-h+ggtitle(paste("6,9 difft validates t.f. limit. Speed limits vary widely",lapseMsg))
show(h)
ggsave( paste0('figs/',tit,'.png') )
########################################Temporal frequency against targets, individual Ss
#p2 <- aes(x=numObjects-1,y=temporalFreq,color=targets); h %+% p2 #quick t.f. plot
### Pattern remarkably consistent across Ss, perhaps show in paper?
tit=paste0(expNames,"_indivSsTemporalFreq ",infoMsg," threeQuarterThresh")
quartz(title=tit,width=6,height=3)
t<-subset(threshes,criterionNote=="threeQuarters") # & exp!="HC2013")
#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
h<-ggplot(data=t, aes(x=numTargets,y=tfThresh,color=objects))
h<-h+facet_grid(. ~ exp)  #facet_grid(criterion ~ exp)
h<-h+ylab('threshold tf (Hz)')
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
dodgeAmt=.3
h<-h+ geom_point(position=position_dodge(width=dodgeWidth)) 
#I don't know why have to specify the group to get the lines to work
h<-h+ geom_line(aes(group=interaction(subject,numObjects)),position=position_dodge(width=dodgeWidth)) 
#h<-h+stat_summary(fun.data = mean_cl_normal, geom="errorbar", mult=1, width=.5, position=position_dodge(width=dodgeWidth))
h<-h+ggtitle(paste(tit,lapseMsg))
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
#j<-h %+% threshes 
#j<-j+ facet_grid(criterion ~ exp)
##########################################tf mean threshes against targets
tit=paste0(expNames,"_tfMeanThreshAgainstTargets ",infoMsg," threeQuarterThresh")
quartz(title=tit,width=6,height=3)
#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
d<-subset(threshes,criterionNote=="threeQuarters")
h<-ggplot(data=d, aes(x=numTargets,y=tfThresh,color=distractors))
h<-h+facet_grid(. ~ exp)  #facet_grid(criterion ~ exp)
h<-h+ylab('threshold tf (Hz)')
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
h<-h+scale_x_continuous(breaks=c(1,2,3))
dodgeAmt=.25
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeAmt))
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeAmt))
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeAmt)) 
h<-h+ggtitle(paste(tit,lapseMsg))
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
##########################################tf mean threshes against targets multiple criteria
quartz(title=tit,width=6,height=8)
tit=paste0(expNames,"_tfMeanThreshAgainstDistractors ",infoMsg,"_specialCriteria")
special<-subset(threshes, criterionNote!="nothingSpecial") #criterion=0.85)
special$targets<-as.factor(special$numTargets); special$objects<-special$numObjects
j<-h %+% special
j<-j+ facet_grid(exp ~ criterionNote)
show(j) #Decline at 9 objects is more consistent in 3/4 and 6/7 thresh than halfway thresh
#If you could somehow argue that 6/7 is appropriate for 6 objects but 3/4 for 9 objects,
#you wouldn't see the fall.
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
##########################################tf individual Ss against distractors
tit=paste0(expNames,"_tfSsAgainstDistractors ",infoMsg," threeQuarterThresh")
quartz(title=tit,width=6,height=3) 
#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
h<-ggplot(data=subset(threshes,criterionNote=="threeQuarters"),   #midpoint
          aes(x=numObjects,y=tfThresh,color=subject)) #color=targets,
h<-h+facet_grid(exp~targets)  #facet_grid(criterion ~ exp)
h<-h+ylab('threshold tf (Hz)')
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
xTicks= unique(threshes$numObjects-1) #put axis ticks at actual values used
h<-h+scale_x_continuous(breaks=c( xTicks ))
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
dodgeAmt=.3
h<-h+ geom_point()
h<-h+ geom_line()
h<-h+ggtitle(paste("individ Ss all show speed-limited for few distractrs, not much flattening by 3 targets",lapseMsg))
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
##########################################################

cat('I give you threshes')
###################################
#plot thresholds all criteria levels I DONT THINK I CAN DO THAT BECAUSE DIFFERENT ONES FOR DIFFERENT
#NUM TARGETS
tit=paste0(expNames,"_tfMeanThreshAgainstTargets ",infoMsg)
quartz(title=tit,width=6,height=3)
#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
d<-subset(threshes,criterionNote!="nothingSpecial")
d<-threshes
h<-ggplot(data=d, aes(x=numObjects,y=tfThresh,color=targets))
h<-h+facet_grid(criterion ~ exp)
h<-h+ylab('threshold tf (Hz)')
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
dodgeAmt=.25
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeAmt))
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeAmt))
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeAmt)) 
h<-h+ggtitle(paste(tit,lapseMsg))
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
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
