#working directory set by starting Rstudio via .Rproj file
#setwd("/Users/alexh/Documents/attention_tempresltn/multiple object tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/speed-tf-VSS14")
source('helpers/psychometricHelpRobust6.R') #for makeMyPsychoCorr, 

meanIfNumber<-function(x) {if (is.numeric(x)) return (mean(x))  else return (unique(x)[1]) }
colsToDelete=c("nErrs","temp","nWarns","firstWarn","error","targets") #will use targets instead of numTargets

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

speedLimitEachSubject<-speedLimitEachSubject[ , !names(speedLimitEachSubject) %in% colsToDelete] 

tfLimitEachSubject<-tfLimitEachSubject[ , !names(tfLimitEachSubject) %in% colsToDelete] 
###################################################################################################
#Create predicted psychometric curve for each condition I'm interested in, based on theoretical speed limit
numSpeeds=150 #250
speeds<-seq(0.04,4,length.out=numSpeeds)
nTarg=c(1,2,3)
conditns= expand.grid( targets=nTarg, numObjects=c(2,3),
                       speed=speeds,subject=unique(speedLimitEachSubject$subject) )

psychometricsSpeed= speedLimitEachSubject
#replicate for each number of targets
rn = row.names(psychometricsSpeed)
psychometricsSpeed= psychometricsSpeed[ rep(rn, times=length(nTarg)),  ] #replicate whole thing for each target num
psychometricsSpeed$targets = nTarg[ rep(seq(1,length(nTarg)), each=length(rn)) ] #rep each element 
row.names(psychometricsSpeed)= NULL
psychometricsSpeed$numObjects=NULL #will be replaced, padded-out by conditns
psychometricsSpeed$numTargets=NULL #will use conditns' targets

psychometricsSpeed = merge(psychometricsSpeed,conditns)
psychometricsSpeed$chanceRate = 1/psychometricsSpeed$numObjects

#Create predicted psychometric curve for each condition I'm interested in, based on theoretical tf limits
psychometricsHz= tfLimitEachSubject
#includes separate limit for each target number
row.names(psychometricsHz)=NULL
psychometricsHz$numObjects=NULL #will be replaced by conditns
psychometricsHz$targets = psychometricsHz$numTargets
psychometricsHz$numTargets=NULL #will use conditns' targets
#psychometricsHz= cbind(psychometricsHz,conditnsEachSubject)
psychometricsHz= merge(psychometricsHz,conditns)
psychometricsHz$chanceRate = 1/psychometricsHz$numObjects
##########################calculate predicted %correct
psychoCorr<- makeMyPsychoCorr2("speed")
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
#psychometrics= subset(psychometricsSpeed, targets==1 & numObjects==2 & subject=="FHL") #slopeThisCrit -1.99, thresh=1.96 #debugOFF

tit<-'Both_rps_and_Hz_limits'
quartz(tit,width=4,height=4)
g=ggplot(data=psychometrics,
    aes(x=speed,y=correct,color=limit,shape=subject)) 
g=g+geom_point()+theme_bw()
g=g+geom_line()
g=g+facet_grid(targets~numObjects)
g=g+ylab('Proportion Correct')
g=g+xlab('Speed (rps)') 
g=g+ggtitle('overlap much greater for 3-object case')
show(g)
ggsave( paste('figs/',tit,'.png',sep='') )

#################################################################################
#Multiply it with each other, then extract resulting threshes
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
  
  if ((speedParms$numObjects != tfParms$numObjects)) {
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

#psAfterBoth= ddply(psychometrics,factors,function(df){dg<<-df; STOP})

# ddply(psychometrics,factors,function(df){
#                print(nrow(subset(df,limit=="speed")))})

psAfterBoth= ddply(psychometrics,factors,afterBothLims)
psAfterBoth$limit= "combined"

#Try to merge psAfterBoth into psychometrics so will automatically plot all.
psychometricsLims= rbind(psychometrics,psAfterBoth)
#psychometricsLims= subset(psAfterBoth,numObjects==3)

tit<-'rps_and_Hz_limits_combined'
quartz(tit,width=5,height=3.5)
g=ggplot(data=psychometricsLims, alpha=.5,
      aes(x=speed,y=correct,color=limit,shape=subject))
g=g+geom_point()
g=g+geom_line(aes(size=as.factor(limit)))+theme_bw()
g=g+scale_size_manual(values=c(3,1,1)) #combined thickest
g=g+facet_grid(targets~numObjects)
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

#Extract threshes from model curves. 
threshes <- data.frame()
threshCriteria<-c(0.75)
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
failedConds=threshes[is.na(threshes$thresh),1:2]
if (nrow(failedConds)>0) {
  cat('Failed to find thresh from psychometric for conditions:')
  print(failedConds)
}

toMakeLine= subset(threshes,!is.na(thresh)) #omit where couldn't extract thresh
threshLines= ddply(toMakeLine,factorsPlusLimitType,threshLine)
#threshLines=subset(threshLines,!is.na(speed)) #tf 2 objects 2 rings cut off
minY=min(threshLines$correct) #replace minimum value with higher
threshLines$correct[(threshLines$correct==minY)] = 0.3 #because don't want to show axis all way to 0
threshLines$speed[(threshLines$speed==0)] = min(psychometricsLims$speed) #because don't want to show axis all way to 0
#I have no idea why have to add these separately to prevent lines not getting messed up
g<-g+geom_line(data=threshLines,lty=3)
#g<-g+geom_line(data=threshLines[c(1:6,10:15),],lty=3)
#g<-g+geom_line(data=threshLines[c(7:9),],lty=3)
g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
show(g)
ggsave( paste('figs/',tit,'.png',sep='') )


#THEN PRETTIFY FOR CONCEPTUAL FIGURE FOR PAPER
#reorder factor levels
threshes$limit = factor(threshes$limit,unique(threshes$limit)[c(2,3,1)]) #speed,tf,combined

#I need a plot of the thresholds too, not the psychometric functions
quartz(tit,width=3.2,height=3.5)
g=ggplot(threshes, aes(
     x=factor(numObjects),y=thresh,color=limit,alpha=limit,shape=subject))
g=g+theme_bw()+ facet_grid(targets~.)
dodgeAmt=0.2
g=g+geom_point(size=2.5,position=position_dodge(width=dodgeAmt))
combindOnly=subset(threshes,limit=="combined")
g=g+geom_line(data=combindOnly,aes(group=subject),position=position_dodge(dodgeAmt))
#g=g+geom_line() #doesn't work
g=g+scale_alpha_manual(values=c(.6,.6,1)) #combined thickest
g=g+scale_color_manual(values=c("blue","red","black")) #make combined black
g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
show(g)

showIndividData=TRUE
if (!showIndividData) {
  tit<-'threshesTheory'
  quartz(tit,width=4.5,height=4)
  h=ggplot(threshes,aes(x=limit,y=thresh,shape=subject))
  h=h+theme_bw()+ facet_grid(targets~numObjects)
  dodge=position_dodge(width=0.2)
  h=h+stat_summary(fun.data="mean_cl_boot",aes(group=targets),geom="errorbar",conf.int=.67,width=.2)
  h=h+stat_summary(fun.y=mean,geom="point",aes(group=targets),size=2.5)
  h=h+stat_summary(fun.y=mean,aes(group=targets),geom="line")
  show(h)
} else {
  tit<-'threshesTheoryEachSubject'
  quartz(tit,width=8,height=7)  #(tit,width=4,height=3.5)
  h=ggplot(threshes,aes(x=limit,y=thresh,shape=subject))
  h=h+theme_bw()+ facet_grid(targets~numObjects)
  dodgeAmt=0.2
  h=h+geom_point(size=2.5,position=position_dodge(width=dodgeAmt))
  h=h+geom_line(aes(group=subject),position=position_dodge(dodgeAmt))
  show(h)
}
###############################################################################
#Now plot cost of going from 1 distractor to 2

#Is 2-target observed threshold (3.9 Hz) predicted by combo of 4.4 Hz and 1-target speed limit?

#For doing stats, need to do it for each subject's psychometric function in each condition,
#unless that's overkill because have some unstable Ss. But ideally would show that Ss with
#particularly low t.f. limit also have low speed - 3 distractors limit

#For 2 targets and 3 targets, which limits, speed or temporal frequency?
#Can decrease in speed limit be accounted for entirely by t.f. decrease even with 1 distractor?
###############################################################################
#Now plot empirical thresholds on the model plot
#
#1-target 2 objs assumed to be true speed limit. Let's see if decrease with targets can be
#explained by 2-target, 3-target tf limits
twoObjs123targets = subset(threshes_speed_123targets269objects,numObjects==2)# & numTargets>1)

#Check thresholds gotten originally for 1target2objs matches new limits found by
#generating psychometric function from params, then extracting thresholds
#I'm only re-extracting thresholds because I got new combined curves, but incidentally re-extract
#From one that's already been extracted.
newThresh2Objs1target = subset(threshes,targets==1 & numObjects==2 & limit=="speed")
oldThresh2Objs1target = subset(twoObjs123targets, numTargets==1 & numObjects==2 )
colsToCompare=c("numObjects","targets","subject","criterion","thresh","slopeThisCrit")
oldThresh2Objs1target=oldThresh2Objs1target[,colsToCompare]
newThresh2Objs1target=newThresh2Objs1target[,colsToCompare]
if (any(newThresh2Objs1target$criterion != oldThresh2Objs1target$criterion)) {
  warning("You used a different criterion to extract predicted thresholds")
  stop("You used a different criterion to extract predicted thresholds")
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

twoObjs123targets$limit= "combined"
#h+stat_summary(fun.y=mean,geom="point",dat=TwoObjs2_3targets,color="red")

#Extend red line in 1-target case to speed side, because it's identical
actual1target = subset(twoObjs123targets,targets==1)
actual1targetDuplicate=actual1target
actual1target$limit="speed"
actual1target= rbind(actual1target,actual1targetDuplicate)
h=h+geom_point(dat=subset(actual1target,limit=="combined"),aes(group=subject),color="red")
h=h+geom_line(dat=actual1target,aes(group=subject),color="red") #draw a line to show it's meaningless in 1-target case

twoObjs23targets=subset(twoObjs123targets,targets>1)
h=h+geom_point(dat=twoObjs23targets,aes(group=subject),color="red",position=position_dodge(width=0.6))
#h=h+stat_summary(dat=TwoObjs2_3targets,fun.data="mean_cl_boot",geom="errorbar",conf.int=.67,
#               position=position_dodge(width=0.6),color="red",width=.2) 
#+geom_line(data=2objs2_3targets,aes(group=subject),color="red")

captn="Fit with lapse rate"
if (length(unique(threshes_tf_123targets269objects$lapseRate))==1) {
  captn=paste(captn,as.numeric(unique(threshes_tf_123targets269objects$lapseRate)[1]))
} else captn=paste(captn,'variable.')
captn=paste(captn,"Red points = actual threshs. Black combined are theoretical, based on tf limit decline with targets")
h=h+ ggtitle(captn) +theme(plot.title=element_text(size=7))
#theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29))
show(h)
tit<-paste(tit,'withActualResult',sep='')
ggsave( paste('figs/',tit,'.png',sep='') )
dev.set(dev.prev()) #because ggsave changes which window active, but I want to modify more

#######add 2->3 objects actual finding
#Dammit only method of adjustment used both 2 and 3 objects. So, not comparable. But,
load("data/E2_CRT_spinzter.Rdata",verbose=TRUE)
#can plot its thresholds for 2 vs. 3 objects to see if decrement comparable
methodAdjst23objs = dat# subset(dat, device=="CRT")
#But the method of adjustment won't be subject to difference in chance rate. So, really need
#objective experiment.
#Is objective part most comparable to adjustment midpoint threshold or what?
methodAdjst23objs$targets = 1
methodAdjst23objs$limit = "heck"
methodAdjst23objs[ methodAdjst23objs$numObjects==2, ]$limit="speed"
methodAdjst23objs[ methodAdjst23objs$numObjects==3, ]$limit="combined"

#aggregate across ecc, device, direction, trackRing?
meanAcrossAdjst<-aggregate(methodAdjst23objs, by=list(
    tmp=methodAdjst23objs$limit,tmp2=methodAdjst23objs$subject,tmp3=methodAdjst23objs$numObjects), meanIfNumber) #average whole thing

meanAcrossAdjst[meanAcrossAdjst$numObjects==2,]$numObjects=3 #this isn't really right because doesn't adjust for chance, but shoehorns onto modeling graph 
#Can't label subject with shape because too many additional subjects
#Not worth it anyway for incomparable method of adjustment
h=h+geom_point(dat=meanAcrossAdjst,shape=1,color="blue",position=position_dodge(width=0.6))
show(h)
tit<-paste(tit,'withMethodAdjstment',sep='')
ggsave( paste('figs/',tit,'.png',sep='') )

#h+geom_line(data=meanAcrossAdjst,aes(shape=subject),color="blue",lty=2,position=position_dodge(width=0.6))
#h+geom_line(data=meanAcrossAdjst,aes(group=subject),color="blue",lty=2,position=position_dodge(width=0.6))
#verdict: decrease is about what would predict. I guess I should present methodAdjstment as something to validate a traditnl CRT experiment

############################################################################
#PLOT PREDICTED TARGET DECLINE AND ACTUAL
tit<-'PredictedEachTargetsEachSubject'
predictdAndTfAndActual = subset(threshes_speed_123targets269objects,numObjects==2 | numObjects==6)# & numTargets>1)
predictdAndTfAndActual$limit="actual"
colsToKeep=c("numObjects","targets","subject","criterion","thresh","slopeThisCrit","error","limit")
predictdAndTfAndActual=predictdAndTfAndActual[,colsToKeep]
predictd=subset(threshes,limit=="combined")
predictd$limit="predicted"
predictdAndTfAndActual=rbind(predictdAndTfAndActual,predictd)
tf=subset(threshes,limit=="tf")
speed=subset(threshes,limit=="speed")
predicted=rbind(predictdAndTfAndActual,tf,speed)

#show individual Ss.  Too crowded
quartz(tit,width=4.5,height=4)
h=ggplot(predicted,aes(x=targets,y=thresh,color=limit,shape=subject))
dodgeIt=position_dodge(width=0.4)
h=h+geom_point(position=dodgeIt)
h=h+geom_line(aes(group=interaction(subject,limit)),position=dodgeIt)
h=h+theme_bw()+facet_grid(.~numObjects)
h

#average across Ss
h=ggplot(predicted,aes(x=targets,y=thresh,color=limit))
dodgeIt=position_dodge(width=0.3)
h=h+stat_summary(fun.data="mean_cl_boot",fun.y="mean",geom="errorbar",conf.int=.67,position=dodgeIt)
h=h+stat_summary(data=predicted,fun.y="mean",geom="line",aes(group=limit),position=dodgeIt)
h=h+theme_bw()+facet_grid(.~numObjects)
h=h+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
h=h+scale_color_manual(values=c("black","red","blue","green")) #make combined black
h
captn="Actual is much worse than predicted, suggesting that speed limit declines with targets"
h=h+ ggtitle(captn) +theme(plot.title=element_text(size=7))
h
#REALLY SHOULD TRY THIS FOR A FEW DIFFERENT THRESHOLD CRITERIA
#ALSO CHECK THAT THE 6-OBJECT CASE IS CORRECTLY PREDICTED. PRESUMABLY YES

#Plot amount decrement expected vs. amount found
#Eventually, might have non-method-adjustment numbers for 2 vs. 3 objects too

#Check out the slope of combined predicted versus ACTUAL!! :)
