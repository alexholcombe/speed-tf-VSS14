setwd("/Users/alexh/Documents/attention_tempresltn/multiple object tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/analyzeSlopesOfPreviousPapers")
load("threshes_speedSave",verbose=TRUE) #from HolcombeChen2013
load("threshes_tfSave",verbose=TRUE) #from HolcombeChen2013
setwd("/Users/alexh/Documents/attention_tempresltn/multiple object tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/modelComboOfSpeedAndTFlimit")
source('psychometricHelpRobust6.R') 
lapseRate = .01

#Get the speed limit psychometric function. Based on 2 distractors mean across Ss
iv="speed"
twoDistractors<-subset(threshes_speedSave,numTargets==1 & numObjects==3)
twoDistractors<-subset(twoDistractors,criterion==0.8) #simply because criteria redundant with regrds to psychometric function parameters
#For this, calculate average "mean" and "slope"
meanIfNumber<-function(x) {if (is.numeric(x)) return (mean(x))  else return (unique(x)[1]) }
twoDistractors<-aggregate(twoDistractors, by=list(temp=twoDistractors$numObjects), meanIfNumber)
rpsParms<- twoDistractors
colsToDelete=c("nErrs","temp","nWarns","firstWarn","subject","error","targets") #will use targets instead of numTargets
rpsParms<-rpsParms[ , !names(rpsParms) %in% colsToDelete] 
rpsParms$lapseRate<-lapseRate
rpsParms$slopeLabel="mean"

#Now simulate that for all conditions I'm interested in.
#Duplicate the speed limit parameters for every condition of interest.
numTargConds=c(1,2,3)  
#one rpsParms for each level of numTargets. ASSUMING SPEED LIMIT UNAFFECTED BY NUM TARGETS
rpsParms<- rpsParms[ rep(row.names(rpsParms),times=length(numTargConds)),  ] #replicate
rownames(rpsParms)<-NULL
rpsParms$numTargets= rep(numTargConds,each=nrow(twoDistractors))
rpsParms$chanceRate = 1/rpsParms$numObjects

lengthSoFar=nrow(rpsParms)
nObjConditns=c(2,3)  #c(2,3,6)
#Replicate rpsParms for each nObj
rpsParms<- rpsParms[ rep(row.names(rpsParms),times=length(nObjConditns)),  ] #replicate
rownames(rpsParms)<-NULL
rpsParms$numObjects= rep(nObjConditns,each=lengthSoFar)
rpsParms$chanceRate = 1/rpsParms$numObjects

cat('Have replicated rpsParms for all',nrow(rpsParms),'conditions')

numSpeeds=100
speeds<-seq(1,3,length.out=numSpeeds)
#replicate every row of rpsParms for each speed
psychometricsSpeed<- data.frame(rpsParms[,],speed=rep(speeds,each=nrow(rpsParms)))
#psychometricsSpeed<- data.frame(rpsParms[,],speed=rep(1:numSpeeds,each=nrow(rpsParms)))
psychoCorr<- makeMyPsychoCorr2("speed")
#psychoCorr(psychometricsSpeed[1,]) #test it
psychometricsSpeed$myKey= 1:nrow(psychometricsSpeed)
#calculate predicted %correct
psychometricsSpeed$correct= daply(psychometricsSpeed,.(myKey),psychoCorr)
psychometricsSpeed$myKey=NULL

#Get empirical temporal frequency limit psychometric function. Based on 9,12 objs avg across Ss
iv="tf"
tfLimitParms<-subset(threshes_tfSave,(numObjects==9 | numObjects==12)) #& numTargets==1 
tfLimitParms<-subset(tfLimitParms,criterion==0.8) #simply because criteria redundant with regrds to psychometric function parameters
#For this, calculate average "mean" and "slope"
tfLimitParms<-aggregate(tfLimitParms, by=list(temp=tfLimitParms$numTargets), meanIfNumber)
tfLimitParms$lapseRate<-.01
tfLimitParms$slopeLabel="mean"

colsToDelete=c("nErrs","temp","nWarns","firstWarn","subject","error","targets") 
tfLimitParms<-tfLimitParms[ , !names(tfLimitParms) %in% colsToDelete] 

HzParms<-tfLimitParms
#Replicate HzParms for each nObj
HzParms<- HzParms[ rep(row.names(HzParms),times=length(nObjConditns)),  ] #replicate
rownames(HzParms)<-NULL
HzParms$numObjects= rep(nObjConditns,each=nrow(tfLimitParms))
HzParms$chanceRate = 1/HzParms$numObjects 

#Replicate HzParms for each speed, then set speed
psychometricsHz<- data.frame(HzParms[,],speed=rep(speeds,each=nrow(HzParms)))
#I need to multiply by numObjects

#Calculate predicted %correct, using psychometric function parameters
psychometricsHz$myKey= 1:nrow(psychometricsHz)
psychoCorr<- makeMyPsychoCorr2("tf") #will use tf as independent variable
psychometricsHz$tf= psychometricsHz$speed * psychometricsHz$numObjects 
psychometricsHz$correct= daply(psychometricsHz,.(myKey),psychoCorr)
psychometricsHz$myKey=NULL

psychometricsSpeed$limit<-"speed"
#give it same number of columns as Hz by adding tf
psychometricsSpeed$tf=psychometricsSpeed$speed*psychometricsSpeed$numObjects
psychometricsHz$limit<-"tf"
psychometrics= rbind(psychometricsSpeed,psychometricsHz)

tit<-'Both_rps_and_Hz_limits'
quartz(tit,width=5,height=7)
g=ggplot(data=psychometrics,
         aes_string(x="speed",y="correct",linetype="factor(numObjects)",color="limit"))
g=g+facet_grid(numTargets~.)
g=g+geom_line()+theme_bw()
g=g+ylab('Proportion Correct')
g=g+xlab('Speed (rps)') 
g=g+ggtitle('overlap much greater for 3-object case')
show(g)
ggsave( paste('figures/',tit,'.png',sep='') )

#################################################################################
#Multiply it with each other, then extract resulting threshes
pAfterBothLimits<- function(p1,p2,numObjects,lapse) {
  #Take guesses out (assumption: high-threshold model) of each p
  #to yield t, the "true function"- probability of successful tracking.
  #Otherwise, have to guess.
  #Then multiply probs together and re-insert guessing rate
  l = lapse
  c = 1/numObjects #chanceRate
  #Derived the below by taking standard psychometric function
  # p = l*c + (1-l)(t + (1-t)*c) #and solving for t
  t1 = (p1 - c) / (1 + l*c - l - c)
  t2 = (p2 - c) / (1 + l*c - l - c)
  #probability don't fall afoul of either limit
  b = t1*t2
  #re-insert lapse rate and guessing
  pAfter = l*c + (1-l)*(b + (1-b)*c)
  #cat('p1=',p1,' p2=',p2,' b=',b, ' pAfter=',pAfter)
  #cat('t1=',t1,' t2=',t2,' b=',b, ' pAfter=',pAfter)
  as.numeric(pAfter)
}

# pAfterBothLimits(0.9896978,0.9889311,3,.01)
# 
# pAfterBothLimits(0.9,0.8,3,.01)
# 
# speeds = seq(.5,2,length.out=4)
# for (s in speeds) {
#   pBasedOnSpeedLimit = psychoCorr(speedLimParms,s)
#   pBasedOnTFlimit = psychoCorr(tfParms,s*nObjs)
#   p = pAfterBothLimits(pBasedOnSpeedLimit,pBasedOnTFlimit,nObjs,.01)
#   cat(pBasedOnSpeedLimit,pBasedOnTFlimit,p,'\n')
# }

#I could merge the parameters for speed and tf, and then 

#Or I could merge the corresponding p's, then myAfterBothLimits
#could operate row-by-row
#So the "after" would be a separate dataframe
#Or I could split by all variables except limittype, so that after fx would recv both

psychoCorrSpeed<- makeMyPsychoCorr2("speed") #will use speed as independent variable
psychoCorrTf<- makeMyPsychoCorr2("tf") #will use tf as independent variable
afterBothLims<-function(df) {
  #assuming df includes columns speed, tf, numObjects, lapseRate
  #expecting one row (limit=="speed") for the speed limit parms
  #and one row (limit=="tf") for the temporal frequency limit parms
  speedParms= subset(df,limit=="speed")
  tfParms= subset(df,limit=="tf")
  stopifnot(nrow(speedParms)>0, nrow(tfParms)>0)
  
  if ((speedParms$numObjects != tfParms$numObjects) | 
        (speedParms$lapseRate!=tfParms$lapseRate)) {
    stop("afterBothLims: Corresponding params not same conditions") }
  pBasedOnSpeedLimit = psychoCorrSpeed(speedParms)
  pBasedOnTFlimit = psychoCorrTf(tfParms)
  nObj = speedParms$numObjects
  lapseRate = speedParms$lapseRate
  pAfterBoth = pAfterBothLimits(pBasedOnSpeedLimit,pBasedOnTFlimit,nObj,lapseRate)
  #cat(pBasedOnSpeedLimit,pBasedOnTFlimit,df$pAfterBoth) #debugOFF
  #only want to return one row, the p predicted after both limits imposed
  speedParms$correct = pAfterBoth
  return(speedParms)
}
factors<-c("numObjects","numTargets","speed")

psAfterBoth= ddply(psychometrics,factors,afterBothLims)
psAfterBoth$limit= "combined"

#Try to merge psAfterBoth into psychometrics so will automatically plot all.
psychometricsLims= rbind(psychometrics,psAfterBoth)
#psychometricsLims= subset(psAfterBoth,numObjects==3)

tit<-'Both_rps_and_Hz_limits'
quartz(tit,width=5,height=3.5)
g=ggplot(data=psychometricsLims, alpha=.5,
         aes_string(x="speed",y="correct",linetype="factor(numObjects)",color="limit"))
g=ggplot(data=psychometricsLims, alpha=.5,
         aes_string(x="speed",y="correct",color="limit"))
g=g+geom_line(aes(size=as.factor(limit)))+theme_bw()
g=g+scale_size_manual(values=c(3,1,1)) #combined thickest
g=g+facet_grid(numTargets~numObjects)
g=g+ylab('Proportion Correct')
g=g+xlab('Speed (rps)') 
g=g+ggtitle('interactn greater for 3-object case')
g<-g+ scale_y_continuous(breaks=c(0,0.5,1))
g=g+scale_color_manual(values=c("black","blue","red")) #make combined black
#g=g+scale_linetype_manual(size=c(10,1,1))

show(g)
#Eventually, need to take each subject's data and get their personal predicted curve?
#All I need is the HzParms and rpsParms with a new psychoCorrThisSpeed
#Then go through all speeds and calculate combined fit. Then plot the threshold

#But if the parameters of the fit came from tf as the variable, need to make sure I'm
#putting that in right when

#Extract threshes from model curves. This is practice
threshCriteria<-c(0.8)
for (threshCriterion in threshCriteria) {
  
  psychometricTemp<-psychometricsLims
  factorsPlusLimitType<-c(factors,"limit")
  factorsPlusLimitType= factorsPlusLimitType[ factorsPlusLimitType!="speed" ] #delete speed
  
  #use point by point search to find the threshold. 
  myThreshGetNumeric= makeMyThreshGetNumerically("speed",threshCriterion)
  
  threshesThisNumeric = ddply(psychometricTemp,factorsPlusLimitType,myThreshGetNumeric) 
  threshesThisNumeric$criterion <- threshCriterion
  #threshesThis<- merge(threshesThisNumeric,fitParms)
  threshesThis<-threshesThisNumeric
  threshes<- rbind(threshes, threshesThis)
}

factorsPlusLimitType<-c(factors,"limit")
factorsPlusLimitType= factorsPlusLimitType[ factorsPlusLimitType!="speed" ] #delete speed

getThreshesFromCurve<- function(psychometrics,factors,iv,criterion) {
  psychometricTemp<-psychometrics
  #use point by point search to find the threshold. 
  myThreshGetNumeric= makeMyThreshGetNumerically("speed",criterion)
  threshesThisNumeric = ddply(psychometricTemp,factorsPlusLimitType,myThreshGetNumeric) 
  threshesThisNumeric$criterion <- criterion
  return (threshesThisNumeric)
}
#Extract thresholds 
threshes <- data.frame()
criterion=0.8
threshesThis<- getThreshesFromCurve(psychometricsLims,factorsPlusLimitType,"speed",criterion)
threshes<- rbind(threshes, threshesThis)

toMakeLine= subset(threshesThis,!is.na(thresh)) #omit where couldn't extract thresh
threshLines= ddply(toMakeLine,factorsPlusLimitType,threshLine)
#threshLines=subset(threshLines,!is.na(speed)) #tf 2 objects 2 rings cut off
mn=min(threshLines$correct) #replace minimum value with higher
threshLines$correct[(threshLines$correct==mn)] = 0.3 #because don't want to show axis all way to 0
#I have no idea why have to add these separately to prevent lines not getting messed up
#g+geom_line(data=threshLines,lty=3)
g<-g+geom_line(data=threshLines[c(1:6,10:15),],lty=3)
g<-g+geom_line(data=threshLines[c(7:9),],lty=3)
#Why is thresh 1.444 - that seems too slow!
g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
show(g)
ggsave( paste('figures/',tit,'.png',sep='') )

#why is above not working for some threshes, e.g. 
# threshesHeck=subset(threshesThis,numObjects==2 & numTargets==1 & numRings==2 & limit=="combined")
# ddply(threshesHeck,factorsPlusLimitType,threshLine)
# threshLines[with(threshLines,order(numObjects,numTargets,numRings,limit,speed)),]

#THEN PRETTIFY FOR CONCEPTUAL FIGURE FOR PAPER

#I need a plot of the thresholds too, not the psychometric functions

#For doing stats, need to do it for each subject's psychometric function in each condition,
#unless that's overkill because have some unstable Ss. But ideally would show that Ss with
#particularly low t.f. limit also have low speed - 3 distractors limit
#For 2 targets and 3 targets, which limits, speed or temporal frequency?
#2 targets looks like temporal frequency still holds for 
