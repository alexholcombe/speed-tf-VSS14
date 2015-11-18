#Intended to be called by doAllAnalyses_e4ab.R, which 
#variables expected:
#thrAll containing thresholds
thr<-thrAll
infoMsg<-""
lapseMsg=""
if (!varyLapseRate)
  lapseMsg=paste("lapseRate always",unique(lapseMinMax))

themeAxisTitleSpaceNoGridLinesLegendBox = theme_classic() + #Remove gridlines, show only axes, not plot enclosing lines
  theme(axis.line = element_line(size=.3, color = "grey"), 
        axis.title.x=element_text(vjust=.10), #Move x axis label slightly away from axis
        legend.key = element_blank(), #don't put boxes around legend bits
        legend.background= element_rect(fill="transparent",color="grey90"), #put big light grey box around entire legend
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        strip.background = element_rect(fill = 'transparent',color='white')
        #strip.text.y= element_text(vjust=0, size=14)  #seems to have no effect
          )
expNames<- paste0( unique(threshes$exp), collapse="" )
#############################################Plot mean tf,speed threshes against distractors
tit<-paste0(expNames,'_MeanAgainstObjects_',infoMsg,'_threeQuarterThresh') 
quartz(title=tit,width=6,height=6) #create graph of threshes with all iv's
thr$objects <- as.numeric(thr$numObjects) #Otherwise can't connect with lines
thr$targets <- as.factor(thr$numTargets) 
d<-subset(thr,criterionNote=="threeQuarters") # & exp!="HC2013")
d$iv<-as.factor(d$iv)
#levels(d$iv) <- c(" ", "  ") #don't show tf, speed facet labels because implied by ylab
al=1.0 #alpha, if want to emphasize speed points, not t.f. points
h<-ggplot(data=d, aes(x=objects,y=thresh,color=targets,fill=iv)) 
h<-h+facet_grid(iv ~ exp, scales="free_y") #"free_y")  
#h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
sz=2.5
h<-h+scale_y_continuous(breaks=seq(0,6)) #No way to set axis labels independently. Could be complicated with a custom axis labeller ?scales::trans_new
h<-h+scale_x_continuous(breaks=seq(2,12,2))
h<-h+theme(axis.title.y=element_text(vjust=0.4)) #Move y axis label slightly away from axis
dodgeWidth<-0
h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth), size=2.5,shape=15)
h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeWidth)) 
h<-h+theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) #default text was too big
h<-h+ggtitle(paste("5,8 difft validates t.f. limit. Speed limits vary widely",lapseMsg))
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
###Contrast only tf and speed
#Show the speed domain with a different contrast, or
#Maybe don't connect the 2,3 data with larger numbers of objects. And use squares instead of circles
tit<-paste0(expNames,'_highlightSpeedDomain_MeanAgainstObjects_',"tf_speed",'_threeQuarterThresh') 
quartz(title=tit,width=6,height=4.5) #create graph of threshes with only speed and tf iv's
thrTfSpd<- subset(thr, iv=="speed" | iv=="tf")
thrTfSpdMoreThan3<- subset(thrTfSpd, objects>3)
k<-h %+% thrTfSpdMoreThan3
k<-k+ylab('Hz                                rps    ')  #ylab('tf (Hz)        speed (rps)')
k<-k+theme_classic()
k<-k+theme(panel.margin.x=unit(.04, "npc"),panel.margin.y=unit(.03,"npc"))
#Now add the <=3 objects conditions back in, as squares
thrTfSpd2an3<- subset(thrTfSpd, objects<=3)
#k<-k+stat_summary(data=thrTfSpd2and3, fun.y=mean,geom="point",shape=b) # Doesn't work
#It seems I can't choose shape with stat_summary, so need to calculate mean so can use geom_point
thrMeans<-dplyr::summarise(group_by(thrTfSpd,objects,targets,exp,iv), thresh =mean(thresh,na.rm=TRUE) )
#Somewhat limited by speed so draw with squares.
k<-k+geom_point(data=subset(thrMeans,objects<4), size=sz, shape=15)
k<-k+geom_line(data=subset(thrMeans,objects<4))
k<-k+stat_summary(data=thrTfSpd2and3,fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95) #error bar has to use non-means
#Drawing dashed line connecting to rest of data is more complicated 
#experiment. Because HC2013 needs to connect to 6 objects
#HC2013speedLimited<- subset(thrMeans,exp=="HC2013" & objects<
k<-k+ stat_summary(data=thrTfSpd2and3,fun.y=mean,geom="line")
k<-k+stat_summary(data=thrTfSpd2and3,fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95) 
#I need a larger vertical spacing between the panels. How to control vertical indepnedn
#Draw rectangles
topPanelYrange<-ggplot_build(k)$panel$ranges[[1]]$y.range
btmPanelYrange<-ggplot_build(k)$panel$ranges[[5]]$y.range
xRange<- ggplot_build(k)$panel$ranges[[1]]$x.range
#Create separate rectangles for iv=tf and iv=speed
tfArea<-data.frame(xmin=3.5, xmax=xRange[2], ymin=topPanelYrange[1], ymax=topPanelYrange[2], limitation="tf",iv="speed")
tfArea<-rbind(tfArea, data.frame(
  xmin=3.5, xmax=xRange[2], ymin=btmPanelYrange[1], ymax=btmPanelYrange[2], limitation="tf",iv="tf") )
whiteRectToBeDroppedButToActivateLegend<- data.frame(
  xmin=4.5,xmax=4.501,ymin=1.2,ymax=1.201,limitation="speed",iv="speed") #,
#  xmin=-2,xmax=-1,ymin=1,ymax=2,limitation="tf",iv="tf")
#How am I going to get rid of the whiteRect? Can't, but make it invisible by having background also be white
rectLims<-rbind(tfArea,whiteRectToBeDroppedButToActivateLegend)
k<-k+geom_rect(data=rectLims, aes(NULL, NULL, xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, 
                   fill = limitation, alpha=0.5),colour=NA)
#Unfortunately, drawing a rectangle always then causes the axis limits to retreat so it doesn't
#go to the border. Can't crop with coord_cartesian differently in different panels, so
#No way around that without hacking the graph direcly by modifying its grobs and
#then rebuilding the plot, which would take a long time to work out. Just be happy with what I got.
k<-k+ scale_fill_manual( values = c("white","grey70") )
k<-k+  theme(legend.key = element_rect(color="grey80")) # put boxes around legend bits
k<-k+theme( panel.background = element_blank(),
            strip.background = element_blank(),
            panel.grid.minor = element_blank(),
            strip.text.y = element_blank(), #Don't need these labels, because units imply them
            axis.line = element_line(size=.3, color = "grey40")
            #axis.text.x = element_text(color="grey30")
            )
k<-k+ guides(fill = guide_legend(title = "limited by" ))
k<-k+ guides(alpha = FALSE ) #Eliminate the alpha legend.
k<-k+ coord_cartesian(xlim=c(0.9,xRange[2])) #Give a little breathing space on left so points stand out more
k
ggsave( paste0('figs/',tit,'.png')) # ,bg="transparent" ) #bg option will be passed to png
############################################
####vertically arrayed. Won't work because the two columns (rps vs. Hz) need different y-axes
#which you can't do in ggplot. Anyway, it looks like crap because the rps numbers are so much 
#lower than the Hz numbers.
j<-h + facet_grid(exp ~ iv, scales="free_y") #"free_y") 
quartz(title=tit,width=3.2,height=6.3) 
show(j)
##########Plot threshes, exp*subject*numTargets*numObjects ################
tit=paste(expNames,"_indivSs_threshesDistractors_",infoMsg,"_threeQuarterThresh",sep='')
quartz(title=tit,width=7,height=4) #create graph of thresholds
h<-ggplot(data=subset(thr,criterionNote=="threeQuarters"),   #midpoint
          aes(x=objects,y=thresh,color=targets))
h<-h+facet_grid(iv ~ exp, scales="free_y")  
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
h<-h+ geom_point() + geom_line(aes(group=interaction(subject,targets))) #plot individual lines for each subject
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
#############################################Plot mean speed threshes against numTargets
tit<-paste0(expNames,"_MeanThreshAgainstTargets_",infoMsg,"_threeQuarterThresh")
quartz(title=tit,width=4,height=3) 
#g<-g+guides(color=guide_legend(title="targets")) #change legend title
d<-subset(thr,criterionNote=="threeQuarters")
h<-ggplot(data=d,  aes(x=targets,y=thresh,color=as.factor(objects)))
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
h<-h+facet_grid(iv ~ exp, scales="free_y")  
h<-h+ stat_summary(fun.y=mean,geom="point")
h<-h+ stat_summary(fun.y=mean,geom="line")
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.95) #error bar
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
