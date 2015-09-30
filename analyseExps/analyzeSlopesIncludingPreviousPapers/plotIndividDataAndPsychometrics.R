#maybe write a function that plots the psychometric functions for a dataset / experiment /criterion,
plotIndividDataAndCurves <- function(df,psychometricCurves) {

}

for (expNum in seq(1,1)) {  #draw individual Ss' data, for each experiment
  title<-paste('E',expNum,' individual Ss data',sep='')
  quartz(title,width=10,height=7)
  thisExpDat <- subset(dat,exp==expNum)
  g=ggplot(data= thisExpDat,
           aes_string(x=iv,y="correct",color="factor(numTargets)",shape="factor(numObjects)"))
  g=g+stat_summary(fun.y=mean,geom="point", position=position_jitter(w=0.04,h=0),alpha=.95)
  g=g+facet_grid(numObjects ~ subject)+theme_bw()
  #g<-g+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
  show(g)
  #draw individual psychometric functions, for only one experiment
  thisPsychometrics <- subset(psychometrics,exp==expNum)
  g=g+geom_line(data=thisPsychometrics)
  g=g+geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
  g=g+ylab('Proportion Correct')
  if (iv=="speed") { g=g+xlab('Speed (rps)') } 
  if (iv=="tf") { g=g+xlab('Temporal frequency (Hz)') } 
  #g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
  #g<-g+ scale_x_continuous(breaks=c(0.5,1.0,1.5,2.0,2.5),labels=c("0.5","","1.5","","2.5"))
  #g<-g+ scale_x_continuous(breaks=speeds)
  show(g)
  ggsave( paste('figures/individPlotsE',expNum,'.png',sep='')  )
}
#################################################################################
#####Plot absolute %corr so you can see that   
calcPctCorrIv <- function(df,iv,ivVals) {
  #You will be sent a data.frame for a particular condition. 
  #But will include both 1-target and 2-target and all speeds tested
  print(df)
  answer<- data.frame(iv=ivVals)
  for (i in 1:length(ivVals)) {
    pctCorr<- calcPctCorrThisIvVal(df,iv,ivVals[i])
    answer$pctCorr[i]<-pctCorr
  }
  return (answer)
}

plotPctCorr = 0  #In speed limits and target load, speed ranges don't overlap
if (plotPctCorr) {
  xs<-seq(from=1.3,to=1.5,by=0.1)
  if (iv=="tf") { xs<-seq(from=2.6,to=3.0,by=0.1) }
	pctCorrEachIvVal<- ddply(datMeans,.(exp,numObjects,subject,numTargets),calcPctCorrIv,iv,xs)
	
	quartz(title="pctCorr each condition modelfree stratify by iv.fig",width=4.5,height=5)
	ll<-ggplot(pctCorrEachIvVal,aes_string(x="numTargets",y="pctCorr",
             linetype="factor(numObjects)",color="factor(iv)"))  +theme_bw()
	#k<-ggplot(subset(costEachSpeed,exp==1),aes(x=separatnDeg,y=cost,color=factor(speed)))  +theme_bw()
	ll<-ll + facet_grid(exp~.) #scales="free" would allow each experiment's yaxis to have a different range/scale
	ll<-ll+stat_summary(fun.y=mean,geom="point")
	ll<-ll+ stat_summary(fun.y=mean,geom="line",size=1)
	ll<-ll+ theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
	ll<-ll+ylab('pctCorr')
	ll<-ll+scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9))
	ll<-ll+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.68,position=position_dodge(width=.02)) #error bar
	show(ll)
	ggsave('figures/pctCorrEachSpeed.png')
}