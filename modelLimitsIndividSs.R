#working directory set by starting Rstudio via .Rproj file
#setwd("/Users/alexh/Documents/attention_tempresltn/multiple object tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/speed-tf-VSS14")
source('helpers/psychometricHelpRobust6.R') #for makeMyPsychoCorr, themeAxisTitleSpaceNoGridLinesLegendBox

meanIfNumber<-function(x) {if (is.numeric(x)) return (mean(x))  else return (unique(x)[1]) }
colsToDelete=c("nErrs","temp","nWarns","firstWarn","error","targets","thresh","slopeThisCrit","temporalFreq") 

#######Try it for E1 threshes123targets269objects of HolcombeChen2014VSS.  threshes gotten by doAllAnalyses.R
#Modeled effect of additional object on speed threshold (taking into account t.f. limit).
#For additional targets, want to check if constraint of lower t.f. limit sufficient to explain decrease of 2-object speed limit with targets
load("data/threshes_speed_123targets269objects.Rdata",verbose=TRUE)
speedLimitEachSubject = subset(threshes_speed_123targets269objects,numTargets==1 & numObjects==2)
load("data/threshes_tf_123targets269objects.Rdata",verbose=TRUE)
#Assume tfLimit changes with targets, but speed limit doesn't.
#Eventually, need to compare to converse prediction that speedLimit changes with targets, but tfLimit doesn't
tfLimitEachSubject = subset(threshes_tf_123targets269objects, numObjects==9)
#oneDistractorMean<-aggregate(oneDistractor, by=list(temp=oneDistractor$numObjects), meanIfNumber) #average whole thing
#eightDistractorsMean<-aggregate(eightDistractors, by=list(temp=eightDistractors$numObjects), meanIfNumber) #average whole thing
actualLimitEachSubject = threshes_tf_123targets269objects

speedLimitEachSubject<-speedLimitEachSubject[ , !names(speedLimitEachSubject) %in% colsToDelete] 
tfLimitEachSubject<-tfLimitEachSubject[ , !names(tfLimitEachSubject) %in% colsToDelete] 
actualLimitEachSubject<-actualLimitEachSubject[ , !names(actualLimitEachSubject) %in% colsToDelete] 
###################################################################################################
#Create predicted psychometric curve for each condition I'm interested in, based on 
#theoretical speed limit. 

#eliminate any copies for each criterion, will use my own criteria
speedLimitEachSubject<-speedLimitEachSubject[ , names(speedLimitEachSubject) != 'criterion']
speedLimitEachSubject<-unique(speedLimitEachSubject)
tfLimitEachSubject<-tfLimitEachSubject[ , names(tfLimitEachSubject) != 'criterion']
tfLimitEachSubject<-unique(tfLimitEachSubject)

#Aggregate so can also plot mean between participants
c1=c("numObjects","numTargets") #variable to preserve, collapse across rest
speedLimitMean<-aggregate(speedLimitEachSubject, by=speedLimitEachSubject[c1], #var to preserve
                                  meanIfNumber) #average whole thing
tfLimitMean<-aggregate(tfLimitEachSubject, by=tfLimitEachSubject[c1], #var to preserve
                          meanIfNumber) #average whole thing
speedLimitMean<-speedLimitMean[c(-1,-2)] #delete first column (created by aggregate)
tfLimitMean<-tfLimitMean[c(-1,-2)] #delete first column (created by aggregate)
speedLimitMean$subject="mean"
tfLimitMean$subject="mean"

psychometricsSpeed= rbind(speedLimitEachSubject,speedLimitMean)
#replicate for each number of targets
rn = row.names(psychometricsSpeed)
psychometricsSpeed= psychometricsSpeed[ rep(rn, times=length(nTarg)),  ] #replicate whole thing for each target num
psychometricsSpeed$targets = nTarg[ rep(seq(1,length(nTarg)), each=length(rn)) ] #rep each element 
row.names(psychometricsSpeed)= NULL
psychometricsSpeed$numObjects=NULL #will be replaced, padded-out by conditns
psychometricsSpeed$numTargets=NULL #will use conditns' targets

numSpeeds=150 #250
maxSpeed = 5 #4 depends on subjects and criterion, how far have to go to fall to threshold
speeds<-seq(0.04,maxSpeed,length.out=numSpeeds)
nTarg=c(1,2,3)
conditns= expand.grid( targets=nTarg, numObjects=c(2,3,6),
                       speed=speeds,subject=unique(psychometricsSpeed$subject) )

psychometricsSpeed = merge(psychometricsSpeed,conditns)
psychometricsSpeed$chanceRate = 1/psychometricsSpeed$numObjects
#Have a column with "observed" or "theory" values so I don't get confused about what's what
psychometricsSpeed$type="theory"
psychometricsSpeed[psychometricsSpeed$targets==1 & psychometricsSpeed$numObjects==2,]$type="observed"

#Create predicted psychometric curve for each condition I'm interested in, based on theoretical tf limit
#experimentally observed for each number of targets
psychometricsHz= rbind(tfLimitEachSubject,tfLimitMean) #includes separate limit for each target number
#eliminate any copies for each criterion, will use my own criteria
psychometricsHz<-psychometricsHz[ , names(psychometricsHz) != 'criterion']
psychometricsHz<-unique(psychometricsHz)
row.names(psychometricsHz)=NULL
psychometricsHz$numObjects=NULL #will be replaced by conditns
psychometricsHz$targets = psychometricsHz$numTargets
psychometricsHz$numTargets=NULL #will use conditns' targets
#psychometricsHz= cbind(psychometricsHz,conditnsEachSubject)
psychometricsHz= merge(psychometricsHz,conditns)
psychometricsHz$chanceRate = 1/psychometricsHz$numObjects
psychometricsHz$type="theory"
if (any(unique(psychometricsHz$numObjects) %in% unique(tfLimitEachSubject$numObjects))) {
  psychometricsHz[psychometricsHz$numObjects %in% unique(tfLimitEachSubject$numObjects),]$type="observed"
}

##########################calculate predicted %correct
psychoCorr<- makeMyPsychoCorr2("speed") #make function calculating %correct for a psychometric fx
psychometricsSpeed$myKey= 1:nrow(psychometricsSpeed)
psychometricsSpeed$correct= daply(psychometricsSpeed,.(myKey),psychoCorr)
psychometricsSpeed$myKey=NULL
psychometricsSpeed$limit<-"speed"

psychoCorr<- makeMyPsychoCorr2("tf") #will use tf as independent variable
#Calculate predicted %correct, using psychometric function parameters
psychometricsHz$myKey= 1:nrow(psychometricsHz)
psychometricsHz$tf= psychometricsHz$speed * psychometricsHz$numObjects 
psychometricsHz$correct= daply(psychometricsHz,.(myKey),psychoCorr)
psychometricsHz$myKey=NULL
psychometricsHz$limit<-"tf"

#give it same number of columns as Hz by adding tf
psychometricsSpeed$tf=psychometricsSpeed$speed*psychometricsSpeed$numObjects

psychometrics= rbind(psychometricsSpeed,psychometricsHz)
psychometrics$distractors = psychometrics$numObjects-1
tit<-'Both_rps_and_Hz_limits' #If you want to see average psychometric function, for now have to go to modelLimits.R
quartz(tit,width=5,height=4)
g=ggplot(data=subset(psychometrics,subject!="mean"),
    aes(x=speed,y=correct,color=limit,shape=subject)) 
#g=g+geom_point()
g=g+geom_line()
g=g+facet_grid(targets~distractors)
g=g+ylab('Proportion Correct')
g=g+xlab('Speed (rps)')
g=g+scale_x_continuous(breaks=c(0,1,2,3))
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
g=g+ggtitle('overlap much greater for 2-distractor case')
show(g)
ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")

tit<-'Mean_rps_and_Hz_limits' 
quartz(tit,width=5,height=4)
g=g %+% subset(psychometrics,subject=="mean")
show(g)
ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")
tit<-'Mean_rps_and_Hz_limits_1target' 
quartz(tit,width=5,height=4/2)
g=g %+% subset(psychometrics,subject=="mean" & targets==1)
show(g)
ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")
#################################################################################
#Multiply limiting psychometric functions together, then extract resulting threshes
pAfterBothLimits<- function(p1,p2,numObjects,lapse1,lapse2) {
  #Take guesses out (assumption: high-threshold model) of each p
  #to yield t, the "true function"- probability of successful tracking.
  #Otherwise, have to guess.
  #Then multiply probs together and re-insert guessing rate
  l1 = lapse1; l2=lapse2;
  c = 1/numObjects #chanceRate
  #Derived the below by taking standard psychometric function
  # p = l*c + (1-l)(t + (1-t)*c) #and solving for t
  t1 = (p1 - c) / (1 + l1*c - l1 - c)
  t2 = (p2 - c) / (1 + l2*c - l2 - c)
  #probability that you don't fall afoul of either limit
  b = t1*t2
  #re-insert lapse rate and guessing
  #Not obvious how to combine lapse rates. Using different lapse rates
  #implies that somehow in tf regime you make more non-speed-related mistakes?!
  #If so, should use something like max(l1,l2)
  #But concept above of different lapse rates doesn't make sense, unless
  #it's a proxy for poor fit or noise. In which case, take average
  l= (l1+l2)/2
  pAfter = l*c + (1-l)*(b + (1-b)*c)
  #cat('p1=',p1,' p2=',p2,' b=',b, ' pAfter=',pAfter)
  #cat('t1=',t1,' t2=',t2,' b=',b, ' pAfter=',pAfter)
  as.numeric(pAfter)
}
#Should I allow different lapse rate for speed limit and tf limit? Doesn't seem to
#make sense if it's the same subject! So should probably fit with constant lapse rate
#originally.

# pAfterBothLimits(0.9896978,0.9889311,3,.01,.01)
# 
# pAfterBothLimits(0.9,0.8,3,.01,.02)
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
  stopifnot(length(unique(speedParms$numObjects))==1, length(unique(tfParms$numObjects))==1)
  if ((speedParms$numObjects[1] != tfParms$numObjects[1])) {
    stop("afterBothLims: Corresponding params not same conditions") }
  pBasedOnSpeedLimit = psychoCorrSpeed(speedParms)
  pBasedOnTFlimit = psychoCorrTf(tfParms)
  nObj = speedParms$numObjects
  #(speedParms$lapseRate!=tfParms$lapseRate)
  l1 = speedParms$lapseRate; l2 = tfParms$lapseRate
  pAfterBoth = pAfterBothLimits(pBasedOnSpeedLimit,pBasedOnTFlimit,nObj,l1,l2)
  #only want to return one row, the p predicted after both limits imposed
  speedParms$correct = pAfterBoth
  return(speedParms)
}
factors<-c("numObjects","targets","speed","subject")
psAfterBoth= ddply(psychometrics,factors,afterBothLims)  #VERY TIME-INTENSIVE ESP. IF FINE SPEED SAMPLING
save.image("temp/tempDoneModelLimitsUpToPsAfterBoth") #load(("temp/tempDoneModelLimitsUpToPsAfterBoth")
psAfterBoth$limit= "combined"; psAfterBoth$type= "theory"
#Add psAfterBoth onto psychometrics so will automatically plot all.
psychometricsLims= rbind(psychometrics,psAfterBoth)
#psychometricsLims$limit= factor(psychometricsLims$limit) #change order so combined plotted first so doesn't overplot
#psychometricsLims$limit <- factor(psychometricsLims$limit, levels(psychometricsLims$limit)[c(2,3,1)])

#Extract threshes from model curves. 
psychometricsLims$criterion= (1.00 + 1 /psychometricsLims$numObjects) / 2.0 #midpoint threshold 

factorsPlusLimit<-c(factors,"criterion","limit")
factorsPlusLimit= factorsPlusLimit[ factorsPlusLimit!="speed" ] #delete speed because gotta have all speeds together
#use point by point search to find the threshold. 
getThreshAndPreserveType <- function(df) {  #otherwise type gets thrown away, want to keep it for plotting color
  stopifnot(length(unique(df$criterion))==1)
  threshCriterion = df$criterion[1]
  #set up thresh-getter for this criterion
  myThreshGetNumeric= makeMyThreshGetNumerically("speed",threshCriterion)
  ansDf = myThreshGetNumeric(df)
  ansDf$criterion = threshCriterion
  type= df$type[1]
  stopifnot(length(unique(df$type))==1)    
  ansDf$type = type
  dj<<-ansDf #debugON
  ansDf
}
threshes = ddply(psychometricsLims,factorsPlusLimit,getThreshAndPreserveType) 
threshes$distractors = threshes$numObjects-1
  
failedConds=threshes[is.na(threshes$thresh),c(3,4,1,2,8)]
if (nrow(failedConds)>0) {
  cat('Failed to find thresh from psychometric for following conditions. Often this happens')
  cat(' due to performance not falling low enough in speed range included.\n')
  print(failedConds)
}
#reorder factor levels
#threshes$limit = factor(threshes$limit)
threshes$limit = factor(threshes$limit,unique(threshes$limit)[c(2,3,1)]) #speed,tf,combined

toMakeLine= subset(threshes,!is.na(thresh)) #omit where couldn't extract thresh
threshLine <- function(df) {   #should be sent a one-row piece of data frame with threshold speed the last column
  #assumes that df has column "thresh"
  threshes = df$thresh
  speeds=c(0,threshes[1],threshes[1])
  yMin=0
  corrects=c(df$criterion,df$criterion,yMin-.2) #draw down to horizontal axis. The -.2 makes sure it extends into margin
  grid<-data.frame(speed=speeds,correct=corrects)
  #print('grid='); print (grid)
  return (grid) 
}
threshLines= ddply(toMakeLine,factorsPlusLimit,threshLine)
#threshLines=subset(threshLines,!is.na(speed)) #tf 2 objects 2 rings cut off
minY=min(threshLines$correct) #replace minimum value with higher
threshLines$correct[(threshLines$correct==minY)] = 0.1 #because don't want to show axis all way to 0
threshLines$speed[(threshLines$speed==0)] = min(psychometricsLims$speed) #because don't want to show axis all way to 0
#threshLines$distractors=as.factor(threshLines$numObjects-1)
threshLines$distractors=threshLines$numObjects-1

tit<-'rps_and_Hz_limits_combined'
quartz(tit,width=5,height=3.5)
g=ggplot(data=subset(psychometricsLims,subject!="mean"), alpha=.5,
      aes(x=speed,y=correct,color=limit,shape=subject))
#g=g+geom_point()
g=g+geom_line(aes(size=as.factor(limit)),alpha=.75)
g=g+scale_size_manual(values=c(3,1,1)) #combined thickest
g=g+ylab('Proportion Correct')
g=g+xlab('Speed (rps)') 
g=g+scale_x_continuous(breaks=c(0,1,2,3,4))
g=g+ggtitle('interactn greater for 2-distractor case')
g<-g+themeAxisTitleSpaceNoGridLinesLegendBox
#g<-g+scale_y_continuous(breaks=c(0,0.5,1))
g=g+scale_color_manual(values=c("grey50","red","blue")) 
gThreshLines=g+facet_grid(targets~distractors, drop=TRUE)
gThreshLines<-gThreshLines+geom_line(data=threshLines,lty=3,size=1)
#gThreshLines=gThreshLines+facet_grid(targets~numObjects)
show(gThreshLines)
titgMean<-'Mean_rps_and_Hz_limits' 
quartz(titgMean,width=5,height=4)
gMean=g %+% subset(psychometricsLims,subject=="mean") + ggtitle("Mean shows little 1-target limit overlap")
gMean=gMean+geom_line(data=subset(threshLines,subject=="mean" & limit=="combined"),color="black",size=.5,lty=3)
gMean=gMean+facet_grid(targets~distractors)
show(gMean)
ggsave( paste('figs/',titgMean,'.png',sep=''), bg="transparent")
titgMean1target<-'Mean_rps_and_Hz_limits_1target' 
quartz(titgMean1target,width=5,height=4/1.5)
gMean1target=g %+% subset(psychometricsLims,subject=="mean" & targets==1)
gMean1target=gMean1target+facet_grid(targets~distractors)
gMean1target=gMean1target+geom_line(data=subset(threshLines,subject=="mean" & limit=="combined" & targets==1),
                                    color="black",size=.5,lty=3)
show(gMean1target)
ggsave( paste('figs/',titgMean1target,'.png',sep=''), bg="transparent")
titg1targetEachS<-'rps_and_Hz_limits_1target_eachS' 
quartz(titg1targetEachS,width=7.2,height=9)
g=g+xlim(0.9,3.6)
g1target=g %+% subset(psychometricsLims,targets==1)
g1target=g1target+geom_line(data=subset(threshLines, targets==1),
                                    size=.5,lty=3)
g1target=g1target+facet_grid(subject~distractors)
show(g1target)
ggsave( paste('figs/',titg1targetEachS,'.png',sep=''), bg="transparent")
#Cherry-pick Subject TF to show how speed and TF can interact
titg1target1subject<-'rps_and_Hz_limits_1target_1subject' 
quartz(titg1target1subject,width=5,height=6.9)
g1target1subject=g %+% subset(psychometricsLims,targets==1 & subject=="UW")
g1target1subject=g1target1subject+  facet_grid(distractors~.) + coord_cartesian(xlim=c(1.0,3.6))
#g1target1subject=g1target1subject+geom_line(data=subset(threshLines, targets==1 & subject=="UW"), size=1,lty=3,alpha=1)
g1target1subject=g1target1subject+geom_line(data=subset(threshLines, targets==1 & subject=="UW"), aes(size=limit),lty=3,alpha=1)
show(g1target1subject)
ggsave( paste('figs/',titg1target1subject,'.png',sep=''), bg="transparent")
#NEED TO ADD HORIZONTAL LINES FOR THRESH CRITERIA

#But if the parameters of the fit came from tf as the variable, need to make sure I'm
#putting that in right when

#I need a plot of the thresholds (above was the psychometric functions)
quartz(tit,width=6.4,height=3.5)
g=ggplot(threshes, aes( x=distractors,y=thresh,color=limit,alpha=limit,shape=subject))
g=g+theme_bw()+ facet_grid(targets~.) #facet_grid(targets~criterion)
dodgeAmt=0.2
g=g+ylab('threshold speed (rps)')
g=g+geom_point(size=2.5,position=position_dodge(width=dodgeAmt))
combindOnly=subset(threshes,limit=="combined")
g=g+geom_line(data=combindOnly,aes(group=subject),position=position_dodge(dodgeAmt))
#g=g+geom_line() #doesn't work
g=g+scale_alpha_manual(values=c(.6,.6,1)) #combined thickest
g=g+scale_color_manual(values=c("blue","red","black")) #make combined black
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
show(g)

#Check for discrepancies ######################################################## 
#Check thresholds gotten originally for 1target2objs matches new limits found by
#generating psychometric function from params then extracting thresholds
#I'm only re-extracting thresholds because I got new combined curves, but incidentally re-extract
#From one that's already been extracted.
twoObjs123targets = subset(threshes_speed_123targets269objects,numObjects==2)# & numTargets>1)
newThresh2Objs1target = subset(threshes,targets==1 & numObjects==2 & limit=="speed")
oldThresh2Objs1target = subset(twoObjs123targets, numTargets==1 & numObjects==2 )
colsToCompare=c("numObjects","targets","subject","criterion","thresh","slopeThisCrit")
oldThresh2Objs1target=oldThresh2Objs1target[,colsToCompare]
newThresh2Objs1target=newThresh2Objs1target[,colsToCompare]

#setdiff(new,old) check whether any in new are not in old
if (length(setdiff( unique(newThresh2Objs1target$criterion), unique(oldThresh2Objs1target$criterion) ))>0) {
  warning("You used a different criterion to extract predicted thresholds")
  #cat("old=")
}
colsToMerge=c("numObjects","targets","subject","criterion")
cmp= merge(oldThresh2Objs1target,newThresh2Objs1target,by=colsToMerge)
if (any( abs(cmp$thresh.x-cmp$thresh.y) >.03 ) | any( abs(cmp$slopeThisCrit.x-cmp$slopeThisCrit.y) >.1 )) {
  msg=paste("Thresholds extracted now differ from those saved before, by average of",
            as.character(round(mean(abs(cmp$thresh.x-cmp$thresh.y)),2)))
  msgSlopeDiff=paste("Slopes extracted now differ from those saved before, by average of",
                     as.character(round(mean(abs(cmp$slopeThisCrit.x-cmp$slopeThisCrit.y)),2)))
  stop(paste(msg,'\n',msgSlopeDiff))
}
####End check for discrepancies ###########################################
