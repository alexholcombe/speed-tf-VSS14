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
actualLimitEachSubject = threshes_tf_123targets269objects

speedLimitEachSubject<-speedLimitEachSubject[ , !names(speedLimitEachSubject) %in% colsToDelete] 
tfLimitEachSubject<-tfLimitEachSubject[ , !names(tfLimitEachSubject) %in% colsToDelete] 
actualLimitEachSubject<-actualLimitEachSubject[ , !names(actualLimitEachSubject) %in% colsToDelete] 
###################################################################################################
#Create predicted psychometric curve for each condition I'm interested in, based on 
#theoretical speed limit. 
numSpeeds=150 #250
maxSpeed = 5 #4 depends on subjects and criteiron, how far have to go to fall to threshold
speeds<-seq(0.04,maxSpeed,length.out=numSpeeds)
nTarg=c(1,2,3)
threshCriteria<-c(0.75,  (1.00 + 1/6) / 2.0 )
conditns= expand.grid( targets=nTarg, numObjects=c(2,3,6),
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
#Have a column with "observed" or "theory" values so I don't get confused about what's what
psychometricsSpeed$type="theory"
psychometricsSpeed[psychometricsSpeed$targets==1 & psychometricsSpeed$numObjects==2,]$type="observed"

#Create predicted psychometric curve for each condition I'm interested in, based on theoretical tf limit
#experimentally observed for each number of targets
psychometricsHz= tfLimitEachSubject #includes separate limit for each target number
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
factors<-c("numObjects","targets","speed","subject","criterion")
psAfterBoth= ddply(psychometrics,factors,afterBothLims)
psAfterBoth$limit= "combined"; psAfterBoth$type= "theory"
#Add psAfterBoth onto psychometrics so will automatically plot all.
psychometricsLims= rbind(psychometrics,psAfterBoth)

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

#But if the parameters of the fit came from tf as the variable, need to make sure I'm
#putting that in right when

#Extract threshes from model curves. 
factorsPlusLimit<-c(factors,"limit")
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

failedConds=threshes[is.na(threshes$thresh),c(3,4,1,2,8)]
if (nrow(failedConds)>0) {
  cat('Failed to find thresh from psychometric for following conditions. Often this happens')
  cat(' due to performance not falling low enough in speed range included.\n')
  print(failedConds)
}
#reorder factor levels
threshes$limit = factor(threshes$limit,unique(threshes$limit)[c(2,3,1)]) #speed,tf,combined

toMakeLine= subset(threshes,!is.na(thresh)) #omit where couldn't extract thresh
threshLines= ddply(toMakeLine,factorsPlusLimit,threshLine)
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

#I need a plot of the thresholds (above was the psychometric functions)
quartz(tit,width=6.4,height=3.5)
g=ggplot(threshes, aes(
     x=factor(numObjects),y=thresh,color=limit,alpha=limit,shape=subject))
g=g+theme_bw()+ facet_grid(targets~criterion)
dodgeAmt=0.2
g=g+geom_point(size=2.5,position=position_dodge(width=dodgeAmt))
combindOnly=subset(threshes,limit=="combined")
g=g+geom_line(data=combindOnly,aes(group=subject),position=position_dodge(dodgeAmt))
#g=g+geom_line() #doesn't work
g=g+scale_alpha_manual(values=c(.6,.6,1)) #combined thickest
g=g+scale_color_manual(values=c("blue","red","black")) #make combined black
g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
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

thisCrit=threshCriteria[2]
thrThisCrit=subset(threshes,criterion==thisCrit)
showIndividData=TRUE
if (!showIndividData) {
  tit<-'threshesTheory'
  quartz(tit,width=4.5,height=4)
  h=ggplot(thrThisCrit,aes(x=limit,y=thresh,color=type,shape=subject))  
  h=h+theme_bw()+ facet_grid(targets~numObjects)
  dodge=position_dodge(width=0.2)
  h=h+stat_summary(fun.data="mean_cl_boot",aes(group=targets),geom="errorbar",conf.int=.67,width=.2)
  h=h+stat_summary(fun.y=mean,geom="point",aes(group=targets),size=2.5)
  h=h+stat_summary(fun.y=mean,aes(group=targets),geom="line")
} else {
  tit<-'threshesTheoryEachSubject'
  quartz(tit,width=8,height=7)  #(tit,width=4,height=3.5)
  h=ggplot(thrThisCrit,aes(x=limit,y=thresh,color=type,shape=subject))
  h=h+theme_bw()+ facet_grid(targets~numObjects)
  dodgeAmt=0.2
  h=h+geom_point(size=2.5,position=position_dodge(width=dodgeAmt))
  h=h+geom_line(aes(group=subject),position=position_dodge(dodgeAmt))
}
h=h+scale_color_manual(values=c("red","black")) #make theory black
h=h+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
show(h)

###############################################################################
#Now plot cost of going from 1 distractor to 2

#Is 2-target observed threshold (3.9 Hz) predicted by combo of 4.4 Hz and 1-target speed limit?

#ideally would show that Ss with particularly low t.f. limit also have low speed - 3 distractors limit
#For 2 targets and 3 targets, which limits, speed or temporal frequency?
#Can decrease in speed limit be accounted for entirely by t.f. decrease even with 1 distractor?
###############################################################################
#Now plot empirical thresholds on the model plot
#
#1-target 2 objs assumed to be true speed limit. Let's see if decrease with targets can be
#explained by 2-target, 3-target tf limits
#h+stat_summary(fun.y=mean,geom="point",dat=TwoObjs2_3targets,color="red")

#NOW DEAL WITH THIS, HOPEFULLY BY MORE CONSOLIDATED OBSERVED DATA
#actual1target, actualEachSubject

#Extend red line in 1-target case to speed side, because it's identical
actual1target2objs = subset(thrThisCrit,targets==1 & limit=="speed" & numObjects==2)
actual1target2objsDuplicate=actual1target2objs
actual1target2objsDuplicate$limit="combined"
actual1target= rbind(actual1target2objs,actual1target2objsDuplicate)
#Now have both speed and combined, same data, to show they are the same, in red
actual1target=subset(actual1target,criterion==thisCrit)
h=h+geom_point(dat=subset(actual1target,limit=="combined"),aes(group=subject),color="red")
h=h+geom_line(dat=actual1target,aes(group=subject),color="red",lty=2) #draw a line to show it's meaningless in 1-target case
h
#OK so I need to have dataframe each subject's limits thisCrit, possibly with type==observed
if (!(thisCrit %in% unique(threshes_speed_123targets269objects$criterion)))
  stop(paste("The criterion you are plotting",thisCrit,"was not provided by the fitting script"))
actualEachSubject = subset(threshes_speed_123targets269objects,criterion==thisCrit)
actualEachSubject = subset(actualEachSubject,(numObjects>2 | targets>1)) #handled this special case separately, above
actualEachSubject$limit ="combined" #use combined label for actual data just so it plots there

h=h+geom_point(dat=actualEachSubject,aes(group=subject),color="red",position=position_dodge(width=0.6))

captn=paste("Criterion=",as.character(round(thisCrit,3)))
captn=paste(captn,"Fit with lapse rate")
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

#######add 2->3 objects actual finding (from method of adjustment)
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

#aggregate across ecc, device, direction, trackRing
meanAcrossAdjst<-aggregate(methodAdjst23objs, by=list(
    tmp=methodAdjst23objs$limit,tmp2=methodAdjst23objs$subject,tmp3=methodAdjst23objs$numObjects), meanIfNumber) #average whole thing

meanAcrossAdjst[meanAcrossAdjst$numObjects==2,]$numObjects=3 #this isn't really right because doesn't adjust for chance, but shoehorns onto modeling graph 
#Can't label subject with shape because too many additional subjects
#Not worth it anyway for incomparable method of adjustment
h=h+geom_point(dat=meanAcrossAdjst,shape=1,color="purple",position=position_dodge(width=0.6))
show(h)
tit<-paste(tit,'withMethodAdjstment',sep='')
ggsave( paste('figs/',tit,'.png',sep='') )

#
LET'S ADD IN 9 OBJECTS PREDICTED SPEED LIMIT AND TF LIMIT??

#verdict: decrease is about what would predict. I guess I should present methodAdjstment as something to validate a traditnl CRT experiment

###############################################################################################
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
quartz(tit,width=4.5,height=4)
h=ggplot(predicted,aes(x=targets,y=thresh,color=limit))
dodgeIt=position_dodge(width=0.3)
h=h+stat_summary(fun.data="mean_cl_boot",fun.y="mean",geom="errorbar",conf.int=.67,position=dodgeIt)
h=h+stat_summary(data=predicted,fun.y="mean",geom="line",aes(group=limit),position=dodgeIt)
h=h+theme_bw()+facet_grid(criterion~numObjects)
h=h+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
h=h+scale_color_manual(values=c("black","red","blue","green")) #make combined black
h
captn="Actual is much worse than predicted, suggesting that speed limit declines with targets."
captn=paste(captn,"But performance is better than expected for numObjects=6, suggesting crowding?")
h=h+ ggtitle(captn) +theme(plot.title=element_text(size=7))
h
#REALLY SHOULD TRY THIS FOR A FEW DIFFERENT THRESHOLD CRITERIA
#ALSO CHECK THAT THE 6-OBJECT CASE IS CORRECTLY PREDICTED. PRESUMABLY YES

#Plot amount decrement expected vs. amount found
#Eventually, might have non-method-adjustment numbers for 2 vs. 3 objects too

#Check out the slope of combined predicted versus ACTUAL!! :)
