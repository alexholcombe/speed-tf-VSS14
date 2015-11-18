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
###########################################################################################
###Contrast only tf and speed   #########################################
#Show the speed domain with a different contrast
#Do this using both fits extracted with log transformation and those not.
#unlog logSpeed and logTf
thrUnlogd<- subset(thr, iv=="logSpd" | iv=="logTf")
thrUnlogd$thresh<- exp(thrUnlogd$thresh) #unlog it
thrUnlogd[thrUnlogd$iv=="logTf",]$iv="tf" #rename
thrUnlogd[thrUnlogd$iv=="logSpd",]$iv="speed" #rename
#Look at large discrepancies between unlogd and never transformed
toMatch<-c(colnames(thr)[1:4],"iv","criterion","chanceRate","criterionNote")
compare<-merge(thrUnlogd,subset(thr, iv=="speed" | iv=="tf"),by=toMatch)
compare$diff<- compare$thresh.x - compare$thresh.y
compThreeQuar<- subset(compare,criterionNote=="threeQuarters")
compThreeQuar<-compThreeQuar[with(compThreeQuar, order(-diff)), ] #Sort by difference
compThreeQuar[1:6,order(names(compThreeQuar))] #Almost all YP
cat("Number of thresh diffs for log fit versus not log fit greater than one:",
      nrow(subset(compThreeQuar,diff>1))) #only 1
useLog<-TRUE
for (useLog in c(TRUE,FALSE)) {
  if (useLog) {
    thrForFig<-thrUnlogd
  } else { thrForFig<- thr }
  if (useLog) tt<-"unlogd" else tt<-""
  tit<-paste0(expNames,'_highlightSpeedDomain_MeanAgainstObjects_',tt,"tf_speed",'_threeQuarterThresh') 
  quartz(title=tit,width=6,height=4.5) #create graph of threshes with only speed and tf iv's
  thrTfSpd<- subset(thrForFig, iv=="speed" | iv=="tf")
  thrTfSpdMoreThan3<- subset(thrTfSpd, objects>3)
  
  k<-h %+% thrTfSpdMoreThan3
  k<-k+ylab('Hz                                rps    ')  #ylab('tf (Hz)        speed (rps)')
  k<-k+theme_classic()
  k<-k+theme(panel.margin.x=unit(.04, "npc"),panel.margin.y=unit(.03,"npc"))
  #Now add the <=3 objects conditions back in, as squares
  thrTfSpd2and3<- subset(thrTfSpd, objects<=3)
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
  show(k)
  ggsave( paste0('figs/',tit,'.png')) # ,bg="transparent" ) #bg option will be passed to png
}