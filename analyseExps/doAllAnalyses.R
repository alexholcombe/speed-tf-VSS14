#Working directory is set by Rproj file
#It is "/Users/alexh/Documents/attention_tempresltn/multiple\ object\ tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/speed-tf-VSS14"

#dirPrefix is what we'll use for all the current operations
# dirPrefix<-"analyseExps/"
# setwd(dirPrefix)

#load("../data/data123targets269objects.RData",verbose=TRUE) #E1
expName="123targets269objects"
load("data/data123targets269objects.RData",verbose=TRUE) #E1
#dat
dat$tf = dat$speed*dat$numObjects
for (iv in c("speed","tf")) {
  source('analyseExps/analyzeMakeReadyForPlot.R') #returns fitParms, psychometrics, and function calcPctCorrThisSpeed
  #save.image(file="speedLimitsAndTargetLoad.RData") #esp. because might want to analyse this data later as testbed for more robust function fitting
  
  #should also do it normalizing by subjects' speed limits
  source("analyseExps/extractThreshesAndPlot.R") #provides threshes, plots
  
  varName=paste("threshes_",iv,"_",expName,sep='')
  assign(varName,threshes)
  save(list=varName,file=paste("data/",varName,".Rdata",sep='')) #e.g. threshes_tf_123targets269objects.Rdata
}

if (iv=="speed") { #if not, don't bother
  source('analyseExps/plotIndividDataWithPsychometricCurves.R') 
}

#source ( model limits) ??

#E2 #######################################################################
load("data/E2_CRT_spinzter.RData",verbose=TRUE) #E2
E2<-dat
#For CRT data, I need to take mean across trials. Spinzter already is
meanThreshold<-function(df) {  
  thresh<-mean(df$thresh)
  df= data.frame(thresh)
  return(df)
}  
factorsPlusSubject=c("numObjects","subject","direction","ecc","device") 

E2threshes<- ddply(dat, factorsPlusSubject,meanThreshold) #average over trialnum,startSpeed

#then plot
tit<-"E2threshesSpeed"
quartz(title=tit,width=2.8,height=2.9) #create graph of thresholds
g=ggplot(E2threshes, aes(x=numObjects-1, y=thresh, color=device, shape=factor(ecc)))
g<-g + xlab('Number of distractors')+ylab('threshold speed (rps)')
dodgeAmt=0.35
SEerrorbar<-function(x){ SEM <- sd(x) / (sqrt(length(x))); data.frame( y=mean(x), ymin=mean(x)-SEM, ymax=mean(x)+SEM ) }
g<-g+ stat_summary(fun.data="SEerrorbar",geom="point",size=2.5,position=position_dodge(width=dodgeAmt))
#g<-g+ stat_summary(fun.y=mean,geom="point",size=2.5,position=position_dodge(width=dodgeAmt))
g<-g+stat_summary(fun.data="SEerrorbar",geom="errorbar",width=.25,position=position_dodge(width=dodgeAmt)) 
g=g+theme_bw() #+ facet_wrap(~direction)
g<-g+ coord_cartesian( ylim=c(1.0,2.5), xlim=c(0.6,2.4))
g<-g+ scale_x_continuous(breaks=c(1,2))
g<-g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
show(g)
ggsave( paste('figs/',tit,'.png',sep='') )
