#source('analyzeMakeReadyForPlot.R') #returns fitParms, psychometrics, and function calcPctCorrThisSpeed
source('rnc_ggplot2_border_themes_2013_01.r') # Simple extensions for removing graph sides, see http://egret.psychol.cam.ac.uk/statistics/R/extensions/rnc_ggplot2_border_themes_2013_01.r  

#Fittng psychometric functions with penalized likelihood should prevent extremely large slopes
numExtremeSlopes<- sum(threshes$slopeThisCrit < -10) #not a number
if (is.na(numExtremeSlopes)) numExtremeSlopes<-0
if (numExtremeSlopes) {
  writeLines( paste("Your psychometric function fitting yielded",numExtremeSlopes," extremely steep slopes"))
}

#Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
#thrCrit comes from plotSpeedThreshesAndCost
tit<-paste('slopesAt',max(critCriteria$criterion)*100,'andScaled*',iv,sep='' )
quartz(title=tit,width=5,height=4) #create graph of thresholds
h<-ggplot(data=thrCrit,aes(x=numObjects-1,y=-slopeThisCrit,linetype=factor(numRings),color=targets))
h<-h + xlab('Number of distractors')
h<-h+scale_x_continuous(breaks=unique(thrCrit$numObjects)-1)
if (iv=="speed") { h=h+ylab('-slope (pCorr/rps)') } 
if (iv=="tf") { h=h+ylab('-slope (pCorr/Hz)') } 
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
dodgeWidth<-.25
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth)) 
h<-h+ stat_summary(fun.ymax= errBarUpper, fun.ymin= errBarLower, geom = "errorbar",width=.5,position=position_dodge(width=dodgeWidth)) 
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
h<-h+theme_bw()
#h<-h+stat_summary(data=threshesThisNumeric,fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.68,position=position_dodge(width=.2)) #error bar
h<-h+ theme_bw()+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
if (!varyLapseRate)
  h<-h+ggtitle('lapse rate always zero')
show(h)
ggsave( paste('figs/',tit,'.png',sep=''), dpi=300  )
########################################################
############################ convert threshes to other iv without refitting
# tit<-paste('converted to Hz threshes',max(critCriteria$criterion)*100,'andScaled',sep='' )
# quartz(title=tit,width=5,height=4) #create graph of thresholds
# #Slope is fit to be change in percent corr per revolution per second 
# #Want to see if it works better as change in percent corr per Hz HZ HZ HZ HZ HZ HZ HZ HZ
# #So, multiply slope by (1/numObjects rev) / cycle
# thrCrit$slopeHzConverted <- thrCrit$slopeThisCrit * (1/thrCrit$numObjects)
# h<-ggplot(data=thrCrit,aes(x=numObjects-1,y=-slopeHzConverted,color=targets))
# h<-h+ylab('-slope (pCorr/Hz)')
# #ylim(1.4,2.5) DO NOT use this command, it will drop some data
# dodgeWidth<-.25
# h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth)) 
# #h<-h+ stat_summary(fun.y=mean,geom="line",size=1)  #I don't understand why line doesn't work
# h<-h+ stat_summary(fun.ymax= errBarUpper, fun.ymin= errBarLower, geom = "errorbar",width=.5,position=position_dodge(width=dodgeWidth)) 
# #h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.95) #error bar
# h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
# h<-h+theme_bw()
# #h<-h+stat_summary(data=threshesThisNumeric,fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.68,position=position_dodge(width=.2)) #error bar
# h<-h+ theme_bw()+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
# #h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
# if (!varyLapseRate)
#   h<-h+ggtitle('lapse rate always zero')
# show(h)

#title<-paste('E',expNum,'_individual_Ss_data_method=',fitParms$method[1],sep='' )
#ggsave( paste('figs/',title,'.png',sep='')  )

##PUT THE BELOW PLOT ONLY IN THE APPENDIX?
###################################
#plot thresholds (at each criterion level in separate graph) for all experiments at same time

for (thisCriterion in unique(threshes$criterion)) {
  thr<-subset(threshes,exp!=5 | numObjects!=2 )#For E5, exclude numObjects==2 because most ppl run with 3 
  thr<-subset(thr,criterion==thisCriterion)
  #thr<-subset(thr,criterion==criticalCrit)
  tit<-paste('slopes_',thisCriterion*100,'criterion_',iv,sep='' )
  quartz(title=tit,width=5.5,height=6)
  #Make the variable names nicer for the plot
  thr$targets<- as.factor(thr$numTargets)
  thr$exp<- as.factor(thr$exp)
  #levels(thr$exp) <-c("E1","E2","E3","E4","E5","E6")
  thr<-subset(thr,targets!="2P")  
  h<-ggplot(data=thr, aes(x=numObjects-1,y=-slopeThisCrit,color=targets,label=exp))
  h<-h+theme_bw() + xlab("Number of distractors") 
  h<-h+ ylab('-slope')
  #h<-h+scale_y_continuous(breaks=c(1.5,2.0))
  h<-h+ facet_grid(exp~.)  
  h<-h + theme(strip.background = element_rect(fill = 'white',color='white'))
  h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth))
  h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
  h<-h+ stat_summary(fun.ymax= errBarUpper, fun.ymin= errBarLower, geom = "errorbar",width=.5,position=position_dodge(width=dodgeWidth)) 
  #ylim(1.4,2.5) DO NOT use this command, it will drop some data
  #h<-h+coord_cartesian(ylim=c(1.4,2.6)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
  h<-h+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines
  
  h<-h + scale_colour_manual(values = c("gray60","black","red","red")) #Change the colors to black and grey
  h<-h+theme(legend.position=c(0.65,0.85) ) #Put legend inside the plot!
  h<-h+theme(legend.key = element_blank())
  h<-h+ theme(axis.title.y=element_text(vjust=0.3)) #Increase space between y label and axis
  ##Label the experiments my way, and remove automatic strip
  #h<-h+ theme(strip.text.y = element_blank()) +theme(strip.background = element_blank()) 
  show(h)
  #ggsave( paste('figs/',tit,'.png',sep=''), dpi=300  )
}
