#This file analyses anonymized data provided by "loadAnonymiseSaveData.R" in exp-specific directory
#Working directory is set hopefully by Rproj file to directory that it's in.
setwd("/Users/alexh/Documents/attention_tempresltn/multiple\ object\ tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/speed-tf-VSS14/analyseExps")
require("ggplot2")
require("plyr")
expName="123targets269objects"
dataDir="../data/"

expName = "data123targets269objects"
anonDataFilename = paste(dataDir,expName,".Rdata",sep="") 
load(anonDataFilename,verbose=TRUE) #E1 #returns dat
dat$expName = expName
datE1 = dat

expName="postVSS_13targets2349objects" 
anonDataFname= paste(dataDir,expName,".Rdata",sep="") #data/postVSS_13targets2349objects.RData
load(anonDataFname,verbose=TRUE) #returns dat
dat_13targets2349objects = dat
dat_13targets2349objects$expName = expName
colsNotInE1 = setdiff(colnames(dat_13targets2349objects),colnames(datE1))
datE1[,colsNotInE1] = -999 #dummy value
colsNotInThisOne = setdiff(colnames(datE1),colnames(dat_13targets2349objects))
dat_13targets2349objects[,colsNotInThisOne] = -999 #dummy value
dat = rbind(dat_13targets2349objects,datE1)

#Analysed in separate repository
# load("offCenter.RData_symbolic_link",verbose=TRUE) #symbolic link created with ln -s works, but not OSX alias
# offCenter = dat
# iv= 'speed'
# source('analyzeMakeReadyForPlot.R') #returns fitParms, psychometrics, and function calcPctCorrThisSpeed
# source('plotIndividDataWithPsychometricCurves.R')

dat$tf = dat$speed*dat$numObjects
for (iv in c("speed","tf")) {
  cat('Fitting data, extracting threshes, plotting with iv=',iv)
  source('analyzeMakeReadyForPlot.R') #returns fitParms, psychometrics, and function calcPctCorrThisSpeed
  if (iv=="speed") { #if not, don't bother
    source('plotIndividDataWithPsychometricCurves.R')
  }
  #should also do it normalizing by subjects' speed limits
  source("extractThreshesAndPlot.R") #provides threshes, thresh plots

  varName=paste("threshes_",iv,"_",expName,sep='') #combine threshes
  assign(varName,threshes)
  save(list=varName,file=paste(dataDir,varName,".Rdata",sep='')) #e.g. threshes_tf_123targets269objects.Rdata
}

#CRT_spinzter experiment#######################################################################
load( paste0(dataDir,"E2_CRT_spinzter.RData"),verbose=TRUE) #E2
E2<-dat
#For CRT data, I need to take mean across trials. Spinzter already is
meanThreshold<-function(df) {  
  thresh<-mean(df$thresh)
  df= data.frame(thresh)
  return(df)
}  
factorsPlusSubject=c("numObjects","subject","direction","ecc","device") 
#Document that there is no effect of direction of motion
E2threshes<- ddply(E2, factorsPlusSubject,meanThreshold) #average over trialnum,startSpeed
g=ggplot(E2threshes, aes(x=direction, y=thresh, color=device, shape=factor(ecc)))
g<-g + xlab('Distractors')+ylab('threshold speed (rps)')
dodgeAmt=0.35
SEerrorbar<-function(x){ SEM <- sd(x) / (sqrt(length(x))); data.frame( y=mean(x), ymin=mean(x)-SEM, ymax=mean(x)+SEM ) }
#g<-g+ geom_point()
g<-g+ stat_summary(fun.data="SEerrorbar",geom="point",size=2.5,position=position_dodge(width=dodgeAmt))
#g<-g+ stat_summary(fun.y=mean,geom="point",size=2.5,position=position_dodge(width=dodgeAmt))
g<-g+stat_summary(fun.data="SEerrorbar",geom="errorbar",width=.25,position=position_dodge(width=dodgeAmt)) 
g=g+theme_bw() #+ facet_wrap(~direction)
g<-g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
g

#Do statistics on direction of motion
#First collapse within subject for simple t-test and calculate means
CRT<- subset(E2,device=="CRT") #only in CRT experiment was direction of motion tested
factors<-c("subject","direction") 
CRTthreshes<- ddply(E2,factors,meanThreshold) #average over trialnum,startSpeed
CCW=subset(CRTthreshes,direction=="CCW")$thresh
CW=subset(CRTthreshes,direction=="CW")$thresh
t<- t.test(x=CCW,y=CW,paired=TRUE)
paste("Mean CCW=",round(mean(CCW),2),"CW=",round(mean(CW),2),
      "t=",round(t$statistic,3),"p=",round(t$p.value),3)
#Counter-clockwise is faster
options(contrasts=c("contr.sum","contr.poly")) #http://blog.gribblelab.org/2009/03/09/repeated-measures-anova-using-r/
#This matters sometimes (not always). If you don’t do it, your sum of squares calculations may not match what you get, for example, in SPSS, or in many common textbooks on ANOVA (e.g. Maxwell & Delaney). See [obtaining the same ANOVA results in R as in SPSS - the difficulties with Type II and Type III sums of squares] for a more detailed account of why this is.
aov.out = aov(thresh ~ direction + Error(subject/(direction)), data=CRT)

#Then do ANOVA, make sure no interaction with eccentricity or anything

factorsPlusSubject=c("numObjects","subject","ecc","device") 
E2threshes<- ddply(E2, factorsPlusSubject,meanThreshold) #average over trialnum,startSpeed
#plot
tit<-"E2threshesSpeed"
quartz(title=tit,width=2.8,height=2.9) #create graph of thresholds
g=ggplot(E2threshes, aes(x=numObjects-1, y=thresh, color=device, shape=factor(ecc)))
g<-g + xlab('Distractors')+ylab('threshold speed (rps)')
dodgeAmt=0.35
SEerrorbar<-function(x){ SEM <- sd(x) / (sqrt(length(x))); data.frame( y=mean(x), ymin=mean(x)-SEM, ymax=mean(x)+SEM ) }
#g<-g+ geom_point()
g<-g+ stat_summary(fun.data="SEerrorbar",geom="point",size=2.5,position=position_dodge(width=dodgeAmt))
#g<-g+ stat_summary(fun.y=mean,geom="point",size=2.5,position=position_dodge(width=dodgeAmt))
g<-g+stat_summary(fun.data="SEerrorbar",geom="errorbar",width=.25,position=position_dodge(width=dodgeAmt)) 
g=g+theme_bw() #+ facet_wrap(~direction)
g<-g+ coord_cartesian( ylim=c(1.0,2.5), xlim=c(0.6,2.4))
g<-g+ scale_x_continuous(breaks=c(1,2))
g<-g+ theme(axis.title.y=element_text(vjust=0.22))
g<-g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
show(g)
ggsave( paste('figs/',tit,'.png',sep='') )

#source ( model limits) ??
#source analyseSlopes?

