#This file analyses anonymized data provided by "loadAnonymiseSaveData.R" in exp-specific directory
#Working directory is set hopefully by Rproj file to directory that it's in.
setwd("/Users/alexh/Documents/attention_tempresltn/multiple\ object\ tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/speed-tf-VSS14/analyseExps")
require("ggplot2")
require("plyr")
dataDir="../data/"
expName = "data123targets269objects"
anonDataFilename = paste(dataDir,expName,".Rdata",sep="") 
load(anonDataFilename,verbose=TRUE) #E1 #returns dat for first experiment
dat$expName = expName
dat$exp = "4a" #name of experiment in manuscript
datE1 = dat

expName="postVSS_13targets2349objects" #load second experiment
anonDataFname= paste(dataDir,expName,".Rdata",sep="") #data/postVSS_13targets2349objects.RData
load(anonDataFname,verbose=TRUE) #returns dat
dat_13targets2349objects = dat
dat_13targets2349objects$expName = dat$exp #postVSS_13targets2349objects
dat_13targets2349objects$exp = "4b" #to make for short graph labels
colsNotInE1 = setdiff(colnames(dat_13targets2349objects),colnames(datE1))
datE1[,colsNotInE1] = -999 #dummy value
colsNotInThisOne = setdiff(colnames(datE1),colnames(dat_13targets2349objects))
dat_13targets2349objects[,colsNotInThisOne] = -999 #dummy value
#Combine two experiments into single dataframe dat
dat = rbind(dat_13targets2349objects,datE1)
datE4<-dat

#load data from HolcombeChen2013JoV
load("/Users/alexh/Documents/attention_tempresltn/multiple\ object\ tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/analyzeSlopesIncludingPreviousPapers/datHolcombeChen2013.RData",
     verbose=TRUE)
dat$numTargets<-dat$numTracked;  dat$numTracked<-NULL
dat$ringWithTarget0<- NaN; dat$ringWithTarget1<- NaN; dat$ringWithTarget2<-NaN
dat$initialDir0<-dat$Direction0; 
dat$initialDir1<-dat$Direction1; 
dat$initialDir2<-dat$Direction2; 
dat$ringPostCued<-NaN
dat$exp<-"HC2013"; dat$file<-"unknown"
dat$expName<-"HolcombeChen2013"
dat$postCueNumBlobsAway<-NULL; dat$respAdjInner<-NULL; dat$respAdjOuter<-NULL; dat$numRings<-NULL
setdiff(colnames(datE4),colnames(dat))
dat$Hz<-NULL
setdiff(colnames(dat),colnames(datE4))
dat = rbind(dat,datE4)
dat$tf<- dat$numObjects*dat$speed
dat$logSpd<- log(dat$speed)
for (iv in c("speed","tf","logSpd")) {
  cat('Fitting data, extracting threshes, plotting with iv=',iv)
  source('analyzeMakeReadyForPlot.R') #returns fitParms, psychometrics, and function calcPctCorrThisSpeed
  #if (iv=="speed") { #if not, don't bother
    source('plotIndividDataWithPsychometricCurves.R')
  #}
  source("extractThreshes.R") #provides threshes
  varName=paste("threshes_",iv,"_",expName,sep='') #combine threshes
  assign(varName,threshes)
  save(list=varName,file=paste(dataDir,varName,".Rdata",sep='')) #e.g. threshes_tf_123targets269objects.Rdata
  cat("Saved",varName)
}
thrTf<-threshes_tf_postVSS_13targets2349objects; thrTf$iv<-"tf"
#Some three-quarters threshes are NA
# exp numObjects numTargets subject thresh
#  4b          4          3      CF     NA      
#HC2013       12          3      PB     NA     
#HC2013       12          3      SM     NA 

thrTF_wtf<- subset(thrTf,criterionNote=="threeQuarters")
subset(thrTF_wtf, is.na(thresh))
       
thrSp<-threshes_speed_postVSS_13targets2349objects; thrSp$iv<-"speed"
thr<- rbind(thrTf,thrSp)
source("makePlots.R")

#source analyseSlopes?
source("analyzeSlopesIncludingPreviousPapers/analSlopes.R")

#CRT_spinzter experiment#######################################################################
load( paste0(dataDir,"E2_CRT_spinzter.RData"),verbose=TRUE) #E2
E2<-dat
#For CRT data, I need to take mean across trials. Spinzter already is
meanThreshold<-function(df) {  
  thresh<-mean(df$thresh)
  df= data.frame(thresh)
  return(df)
}  
#all factors: "numObjects","subject","direction","ecc","device","startSpeed"
#UNDERSTAND THE EFFECT OF DIRECTION OF MOTION
factorsPlusSubject=c("numObjects","subject","direction","ecc","device") 
#Do statistics on direction of motion
#First collapse within subject for simple t-test and calculate means
factors<-c("subject","direction") 
CRT<-subset(E2,device=="CRT") #only in CRT experiment was direction of motion tested
CRTdirOnly<- ddply(CRT,c("subject","direction"),meanThreshold) #average over trialnum,startSpeed
CCW=subset(CRTdirOnly,direction=="CCW")$thresh
CW=subset(CRTdirOnly,direction=="CW")$thresh
data.frame(ccw=CCW,cw=CW,polarity=sign(CCW-CW)) #5 of 9 participants slightly faster for CCW
paste("Mean CCW advantage=",round(mean(CCW)-mean(CW),4))
t<- t.test(x=CCW,y=CW,paired=TRUE)
paste("Mean CCW=",round(mean(CCW),3),"CW=",round(mean(CW),3),
      "t=",round(t$statistic,3),"p=",round(t$p.value,3))
options(contrasts=c("contr.sum","contr.poly")) #http://blog.gribblelab.org/2009/03/09/repeated-measures-anova-using-r/
#This matters sometimes (not always). If you don’t do it, your sum of squares calculations may not match what you get, for example, in SPSS, or in many common textbooks on ANOVA (e.g. Maxwell & Delaney). See [obtaining the same ANOVA results in R as in SPSS - the difficulties with Type II and Type III sums of squares] for a more detailed account of why this is.
source("anovaReport.R")
aov.out = aov(thresh ~ direction + Error(subject/(direction)), data=CRT)
writeLines( ANOVAreport(aov.out,"direction")$msg ) #direction not significant
aov.out = aov(thresh ~ direction*numObjects + Error(subject/(direction*numObjects)), data=CRT) 
writeLines( ANOVAreport(aov.out,"direction")$msg ) #direction not significant
aov.out = aov(thresh ~ direction*numObjects*ecc + Error(subject/(direction*numObjects*ecc)), data=CRT) 
writeLines( ANOVAreport(aov.out,"direction")$msg ) #direction not significant
#check for interactions
aov.out = aov(thresh ~ direction*numObjects + Error(subject/(direction*numObjects)), data=CRT) 
writeLines( ANOVAreport(aov.out,"direction:numObjects")$msg ) #interaction not significant
aov.out = aov(thresh ~ direction*ecc + Error(subject/(direction*ecc)), data=CRT) 
writeLines( ANOVAreport(aov.out,"direction:ecc")$msg ) #interaction not significant

#DIFFERENCE BETWEEN CRT AND SPINZTER
factors=c("device"); d<- ddply(E2, factors,meanThreshold)
d; paste("Advantage of spinzter= ",d$thresh[2]-d$thresh[1])
#numObjects.  Tiny tiny n-s difference in advantage
factors=c("numObjects","device"); d<- ddply(E2, factors,meanThreshold)
d; paste("Advantage of spinzter 2 vs 3 obj= ",round(d$thresh[2]-d$thresh[4],3),
         "CRT",round(d$thresh[1]-d$thresh[3],3))
#eccentricity. Tiny tiny n-s difference in advantage
factors=c("ecc","device"); d<- ddply(E2, factors,meanThreshold)
d; paste("Advantage of spinzter ecc=4.5 vs 12= ",round(d$thresh[2]-d$thresh[4],3),
         "CRT",round(d$thresh[1]-d$thresh[3],3))
aov.out = aov(thresh ~ numObjects*ecc*device + Error(subject/(device)), data=E2)
summary(aov.out)
aov.out = aov(thresh ~ numObjects*ecc*device + Error(subject/(device*numObjects*ecc)), data=E2)
summary(aov.out) #interactions not significant
#writeLines( ANOVAreport(aov.out,"ecc:device")$msg ) #interaction not significant
#EFFECT OF ECCENTRICITY WILDLY SIGNIFICANT WHEN EXPRESSED IN LINEAR DEG VISUAL ANGLE
E2$threshLinear = E2$thresh*E2$ecc*2*pi
#Make numeric vars a factor, so not treated as regressor (results same, but allows printout of estimated means)
E2f<-E2; E2f$ecc<-as.factor(E2$ecc); E2f$numObjects<-as.factor(E2f$numObjects)
ggplot(E2,aes(x=ecc,y=threshLinear,color=factor(device))) + geom_point()
aov.out = aov(threshLinear ~ numObjects*ecc*device + Error(subject/(device*numObjects*ecc)), data=E2f)
summary(aov.out)
model.tables(aov.out,"means")
#
#MAKE THE FIGURE
tit<-"Spinzter_CRT_threshes"
factorsToNotAvg=c("numObjects","subject","ecc","device")
E2sumry<- ddply(E2, factorsToNotAvg,meanThreshold) #average over trialnum,startSpeed, etc
E2sumry$Distractors = as.factor(E2sumry$numObjects-1)
E2sumry$ecc<- as.factor(E2sumry$ecc)
g=ggplot(E2sumry, aes(x=Distractors, y=thresh, color=device, shape=factor(ecc)))
g<-g + ylab('threshold speed (rps)\n') #newline to create space because vjust not working
dodgeAmt=0.27
SEerrorbar<-function(x){ SEM <- sd(x) / (sqrt(length(x))); data.frame( y=mean(x), ymin=mean(x)-SEM, ymax=mean(x)+SEM ) }
g<-g+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,conf.int=.95,
                  position=position_dodge(width=dodgeAmt)) 
g<-g+ stat_summary(fun.data="SEerrorbar",geom="point",size=2.5,position=position_dodge(width=dodgeAmt))
#g<-g+stat_summary(fun.data="SEerrorbar",geom="errorbar",width=.25,position=position_dodge(width=dodgeAmt)) 
g=g+theme_bw()
g<-g+ coord_cartesian( ylim=c(1.0,2.5), xlim=c(0.6,2.4))
#g<-g+ theme(axis.title.y=element_text(vjust=0.42))
g<-g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
quartz(width=3.2,height=4); g
ggsave( paste('figs/',tit,'.png',sep='') )

#model limits
source("modelLimitsIndividSs.R")
