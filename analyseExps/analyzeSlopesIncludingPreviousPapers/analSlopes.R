#To be called by doAllAnalyses_E4ab.R, thus assumes it is in that, parent, directory

#Assumes 'threshes_tf_postVSS_13targets2349objects.Rdata' and 'threshes_speed_postVSS_13targets2349objects'
#created by doAllAnalyses_E4ab.R
load("../data/threshes_tf_postVSS_13targets2349objects.Rdata",verbose=TRUE)
load("../data/threshes_speed_postVSS_13targets2349objects.Rdata",verbose=TRUE)
thrTf<-threshes_tf_postVSS_13targets2349objects; thrTf$iv<-"tf"
thrSp<-threshes_speed_postVSS_13targets2349objects; thrSp$iv<-"speed"

#Fittng psychometric functions with penalized likelihood should prevent extremely large slopes
numExtremeSlopes<- sum(thrSp$slopeThisCrit < -10) #not a number
if (is.na(numExtremeSlopes)) numExtremeSlopes<-0
if (numExtremeSlopes) {
  writeLines( paste("Your psychometric function fitting yielded",numExtremeSlopes," extremely steep slopes"))
}
thr<- rbind(thrTf,thrSp)
##########################################tf mean threshes against targets
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
ggsave( paste0('figs/',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
