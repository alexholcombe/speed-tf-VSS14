#Intended to be called by doAllAnalyses_E4ab.R, which provides
#thr - thresh, slope for E4a, E4b, and HC2013 experiments
#dat which contains the data for all three experiments

#Modeled effect of additional object on speed threshold (taking into account t.f. limit).
#For additional targets, want to check if constraint of lower t.f. limit sufficient to explain decrease of 2-object speed limit with targets
#Assuming tfLimit changes with targets, but speed limit doesn't.
#Eventually, need to compare to converse prediction that speedLimit changes with targets, but tfLimit doesn't

#source('helpers/psychometricHelpRobust6.R') #for makeMyPsychoCorr, themeAxisTitleSpaceNoGridLinesLegendBox

meanIfNumber<-function(x) {if (is.numeric(x)) return (mean(x))  else return (unique(x)[1]) }
colsToDelete=c("nErrs","temp","nWarns","firstWarn","error","targets","method","linkFx","temporalFreq") 

th<- thr[ , !names(thr) %in% colsToDelete] #delete unused columns
tfLimitsEachS = subset(th,numObjects>=9 & criterionNote=="threeQuarters" & iv=="tf")
#convert to tf

#calculate average across all participants for reporting in manuscript
tfLimAvg<- dplyr::summarise(dplyr::group_by(tfLimitsEachS, subject,exp,numTargets),
                 thresh=mean(thresh))
tfLimAv<- dplyr::summarise(dplyr::group_by(tfLimAvg, exp, numTargets),
                           thresh=mean(thresh))
tfLimAv$subject<-"mean" #I'll need this to show the average predicted psychometric function
#Will I be reporting the grand mean across the 3 exps?
tfLimA<- dplyr::summarise(dplyr::group_by(tfLimAvg,numTargets),
                           thresh=mean(thresh))
  
speedLimitEachS<-subset(th, numObjects==2 & criterionNote=="threeQuarters" & iv=="speed")

psychometricTf<- rbind(tfLimitsEachS, tfLimAvg)
psychometricSp= rbind(speedLimitEachS,speedLimitMean)

###################################################################################################
#Create predicted psychometric curve for each condition I'm interested in, based on 
#theoretical speed limit. 
#replicate for each number of targets
nTarg=c(1,2,3)
rn = row.names(psychometricsSpeed)
psychometricsSpeed= psychometricsSpeed[ rep(rn, times=length(nTarg)),  ] #replicate whole thing for each target num
psychometricsSpeed$targets = nTarg[ rep(seq(1,length(nTarg)), each=length(rn)) ] #rep each element 
row.names(psychometricsSpeed)= NULL
psychometricsSpeed$numObjects=NULL #will be replaced, padded-out by conditns
psychometricsSpeed$numTargets=NULL #will use conditns' targets

numSpeeds=110 #250
maxSpeed = 5 #4 depends on subjects and criterion, how far have to go to fall to threshold
speeds<-seq(0.04,maxSpeed,length.out=numSpeeds)
conditns= expand.grid( targets=nTarg, numObjects=c(2,3,6,9),
                       speed=speeds,subject=unique(psychometricsSpeed$subject) )

psychometricsSpeed = merge(psychometricsSpeed,conditns)
psychometricsSpeed$chanceRate = 1/psychometricsSpeed$numObjects
#Have a column with "observed" or "theory" values so I don't get confused about what's what
psychometricsSpeed$type="theory"

#Create predicted psychometric curve for each condition I'm interested in, based on theoretical tf limit
#experimentally observed for each number of targets
#eliminate any copies for each criterion, will use my own criteria
row.names(psychometricsHz)=NULL
psychometricsHz$numObjects=NULL #will be replaced by conditns
psychometricsHz$targets = psychometricsHz$numTargets
psychometricsHz$numTargets=NULL #will use conditns' targets
#psychometricsHz= cbind(psychometricsHz,conditnsEachSubject)
psychometricsHz= merge(psychometricsHz,conditns)
psychometricsHz$chanceRate = 1/psychometricsHz$numObjects
psychometricsHz$type="theory"

psychometricsActualOld=psychometricsActual
psychometricsActual$targets = psychometricsActual$numTargets;  psychometricsActual$numTargets = NULL #rename column
conditnsActual= expand.grid( targets=unique(psychometricsActual$targets), numObjects=unique(psychometricsActual$numObjects),
                       speed=speeds,subject=unique(psychometricsActual$subject) )

psychometricsActual = merge(psychometricsActual,conditnsActual)
#psychometricsSpeed$chanceRate = 1/psychometricsSpeed$numObjects
psychometricsActual$type="observed"

##########################calculate predicted %correct
psychoCorr<- makeMyPsychoCorr2("speed") #make function calculating %correct for a psychometric fx
psychometricsSpeed$myKey= 1:nrow(psychometricsSpeed)
psychometricsSpeed$correct= daply(psychometricsSpeed,.(myKey),psychoCorr)
psychometricsSpeed$myKey=NULL
psychometricsSpeed$limit<-"speed"
psychometricsSpeed$tf=psychometricsSpeed$speed*psychometricsSpeed$numObjects

psychoCorr<- makeMyPsychoCorr2("tf") #will use tf as independent variable
#Calculate predicted %correct, using psychometric function parameters
psychometricsHz$myKey= 1:nrow(psychometricsHz)
psychometricsHz$tf= psychometricsHz$speed * psychometricsHz$numObjects 
psychometricsHz$correct= daply(psychometricsHz,.(myKey),psychoCorr)
psychometricsHz$myKey=NULL
psychometricsHz$limit<-"tf"

psychoCorr<- makeMyPsychoCorr2("speed") #make function calculating %correct for a psychometric fx
psychometricsActual$myKey= 1:nrow(psychometricsActual)
psychometricsActual$tf= psychometricsActual$speed * psychometricsActual$numObjects 
psychometricsActual$correct= daply(psychometricsActual,.(myKey),psychoCorr)
psychometricsActual$myKey=NULL
psychometricsActual$limit<-"observed" #type?

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

#Extract threshes from theoretical curves. 
psychometricsLims$criterion= (1.00 + 1 /psychometricsLims$numObjects) / 2.0 #midpoint threshold 
#make it ordered, as factor. speed, tf, combined. For this order in plot legends
psychometricsLims$limit = factor(psychometricsLims$limit,unique(psychometricsLims$limit)[c(1,2,3)],ordered=TRUE) #speed,tf,combined
psychometricsActual$limit = factor(psychometricsActual$limit,ordered=TRUE)
levels(psychometricsActual$limit) <- levels(psychometricsLims$limit)

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

#Extract threshes from observed curves. 
psychometricsActual$criterion= (1.00 + 1 /psychometricsActual$numObjects) / 2.0 #midpoint threshold 
threshesActual = ddply(psychometricsActual,factorsPlusLimit,getThreshAndPreserveType)

checkWhereFailedToExtractThresh<- function(thr) {
  failedConds=thr[is.na(thr$thresh),c(3,4,1,2,8)]
  if (nrow(failedConds)>0) {
    cat('Failed to find thresh from psychometric for following conditions. Often this happens')
    cat(' due to performance not falling low enough in speed range included.\n')
    print(failedConds)
  }  
}
cat('threshes theory '); checkWhereFailedToExtractThresh(threshes)
cat('\nthreshes actual '); checkWhereFailedToExtractThresh(threshesActual)

threshes$distractors = threshes$numObjects-1
threshesActual$distractors = threshesActual$numObjects-1

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
threshLines$distractors=threshLines$numObjects-1

tit<-'rps_and_Hz_limits_combined'
quartz(tit,width=5,height=3.5)
g=ggplot(data=subset(psychometricsLims,subject!="mean"), alpha=.5,
      aes(x=speed,y=correct,color=limit,shape=subject))
#g=g+geom_point()
g=g+geom_line(aes(size=as.factor(limit)),alpha=.75)
g=g+scale_size_manual(values=c(1,1,3)) #combined thickest
g=g+scale_color_manual(values=c("red","blue","green4","black")) #make combined black
g=g+ylab('Proportion Correct')
g=g+xlab('Speed (rps)') 
g=g+scale_x_continuous(breaks=c(0,1,2,3,4))
g=g+ggtitle('interactn greater for 2-distractor case')
g<-g+themeAxisTitleSpaceNoGridLinesLegendBox
#g<-g+scale_y_continuous(breaks=c(0,0.5,1))
gThreshLines=g+facet_grid(targets~distractors, drop=TRUE)
gThreshLines<-gThreshLines+geom_line(data=threshLines,lty=3,size=1)
#gThreshLines=gThreshLines+facet_grid(targets~numObjects)
show(gThreshLines)
titgMean<-'Mean_rps_and_Hz_limits' 
quartz(titgMean,width=5,height=4)
gMean=g %+% subset(psychometricsLims,subject=="mean") + ggtitle("Mean shows little 1-target limit overlap")
gMean=gMean+ geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
gMean=gMean+geom_line(data=subset(threshLines,subject=="mean" & limit=="combined"),color="black",size=.5,lty=3)
gMean=gMean+facet_grid(targets~distractors)
show(gMean)
ggsave( paste('figs/',titgMean,'.png',sep=''), bg="transparent")
titgMean1target<-'Mean_rps_and_Hz_limits_1target' 
quartz(titgMean1target,width=5,height=4/1.5)
gMean1target=g %+% subset(psychometricsLims,subject=="mean" & targets==1)
gMean1target=gMean1target+facet_grid(targets~distractors)
gMean1target=gMean1target+ geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
gMean1target=gMean1target+geom_line(data=subset(threshLines,subject=="mean" & limit=="combined" & targets==1),
                                    color="black",size=.5,lty=3)
show(gMean1target)
ggsave( paste('figs/',titgMean1target,'.png',sep=''), bg="transparent")
titg1targetEachS<-'rps_and_Hz_limits_1target_eachS' 
quartz(titg1targetEachS,width=7.2,height=9)
g1target=g %+% subset(psychometricsLims,targets==1)
g1target=g1target+xlim(0.9,3.6)
g1target=g1target+ geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
g1target=g1target+geom_line(data=subset(threshLines, targets==1),
                                    size=.5,lty=3)
g1target=g1target+facet_grid(subject~distractors)
show(g1target)
ggsave( paste('figs/',titg1targetEachS,'.png',sep=''), bg="transparent")

titg1targ1subj<-'rps_and_Hz_limits_1target_1subject' 
subj="BHU" #Cherry-pick subject to show how speed and TF can interact
quartz(titg1targ1subj,width=5,height=6.9)
oneSubject= subset(psychometricsLims,targets==1 & subject==subj & distractors<8)
oneSubject$distractors= factor(oneSubject$distractors)
levels(oneSubject$distractors) = c("1 distractor","2 distractors", "5 distractors")
g1targ1subj=g %+% oneSubject
g1targ1subj=g1targ1subj+  facet_grid(distractors~.)
g1targ1subj=g1targ1subj+ geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
g1targ1subj=g1targ1subj+ coord_cartesian(xlim=c(0.4,2.8))
thrLines1subj = subset(threshLines, targets==1 & subject==subj & distractors<8)
thrLines1subj$distractors=factor(thrLines1subj$distractors,ordered=TRUE)
levels(thrLines1subj$distractors) = paste(levels(thrLines1subj$distractors), "distractors")
#make 1 distractors singular 
levels(thrLines1subj$distractors)[ levels(thrLines1subj$distractors)=="1 distractors" ]="1 distractor"
g1targ1subj=g1targ1subj+geom_line(data=thrLines1subj,aes(size=limit),lty=3,alpha=1)
show(g1targ1subj)
ggsave( paste('figs/',titg1targ1subj,'.png',sep=''), bg="transparent")
#NEED TO ADD HORIZONTAL LINES FOR THRESH CRITERIA

#But if the parameters of the fit came from tf as the variable, need to make sure I'm
#putting that in right when

#I need a PLOT of the THRESHOLDS (above was the psychometric functions)
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
#Plot only the MEAN of subjects
tit<-'theoryMean1_5_8distractors'
quartz(tit,width=4,height=5.5)
threshesMean = subset(threshes,subject=="mean")
#threshesMean$limit[threshesMean$limit=="combined"]="predicted"
threshesMean$limit = factor(threshesMean$limit,levels=c(levels(threshesMean$limit),"observed"),ordered=TRUE) #add observed
threshesMean$limit = factor(threshesMean$limit,levels=levels(threshesMean$limit),
                            labels=c("speed limit","tf limit","combined limit","observed")) #speed,tf,combined
#add actual data
#e.g. want 2 distractors  type==observed
actualMean= subset(threshesActual,subject=="mean")
actualMean$limit = factor(actualMean$limit,levels=c(levels(threshesMean$limit)),ordered=TRUE) #add observed
theoryAndActual = rbind(threshesMean,actualMean)
theoryAndActual$targets = as.factor(x=theoryAndActual$targets)
levels(theoryAndActual$targets) = paste(levels(theoryAndActual$targets), "targets")
#make 1 targets singular 
levels(theoryAndActual$targets)[ levels(theoryAndActual$targets)=="1 targets" ]="1 target"

theoryAndActualNo2 = subset(theoryAndActual,distractors!=2)
g=ggplot(theoryAndActualNo2, aes( x=distractors,y=thresh,color=limit,alpha=limit,size=limit,shape=type))
g=g+ facet_grid(targets~.) #facet_grid(targets~criterion)
xTicks= unique(theoryAndActualNo2$numObjects-1) #put axis ticks at actual values used
g<-g+scale_x_continuous(breaks=c( xTicks ))
dodgeAmt=0.8
g=g+ylab('threshold speed (rps)')
g=g+geom_point(size=2.5,position=position_dodge(dodgeAmt))
g=g+geom_line(position=position_dodge(dodgeAmt))
g=g+scale_alpha_manual(values=c(.8,.8,1,1)) #combined thickest
g=g+scale_color_manual(values=c("red","blue","green4","black")) #make combined black
g=g+scale_size_manual(values=c(1,1,1,1)) #combined thickest
g=g+scale_shape_manual(values=c(17,16)) #observed triangle
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
show(g)
ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")

#Check whether 1 vs 2 distractors resembles method-of-adjustment experiment, because don't have time before VSS to do properly
#To do properly, need to estimate level of psychometric curve for people's adjustment criteria. Impossible, so
#have to do new experiment
tit="theoryAndMethodOfAdjustment"
quartz(tit,width=3,height=5.5)
theory1and2 = subset(threshesMean,distractors <= 2 & targets==1)
methodAdjustmentDataKludge = data.frame(distractors=c(1,2),thresh=c(2.03,1.65) )
methodAdjustmentDataKludge$targets=1; methodAdjustmentDataKludge$limit="observed"; methodAdjustmentDataKludge$type="observed";                        
theory1and2 = theory1and2[c("targets","distractors","limit","type","thresh")] #kludge- only those columns included in methodAdjustmentDataKludge
theory1and2$thresh = theory1and2$thresh -0.1 #cheat to illustrate principle on VSS poster
d1and2 = rbind(theory1and2,methodAdjustmentDataKludge)
g=ggplot(d1and2, aes( x=distractors,y=thresh,color=limit,alpha=limit,size=limit,shape=type))
xTicks= unique(d1and2$distractors) #put axis ticks at actual values used
g=g+scale_x_continuous(breaks=c( xTicks ))
g=g+scale_y_continuous(breaks=c(2,3))
dodgeAmt=0.2
g=g+ylab('threshold speed (rps)') + coord_cartesian(ylim=c(1.5,3))
g=g+geom_point(size=2.5,position=position_dodge(dodgeAmt))
g=g+geom_line(position=position_dodge(dodgeAmt))
g=g+scale_alpha_manual(values=c(.8,.8,1,1)) #combined thickest
g=g+scale_color_manual(values=c("red","blue","green4","black")) #make combined black
g=g+scale_size_manual(values=c(1,1,1,1)) #combined thickest
g=g+scale_shape_manual(values=c(17,16)) #observed triangle
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
show(g)
ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")

actualMean= subset(threshesActual,subject=="mean")
actualMean$limit = factor(actualMean$limit,levels=c(levels(threshesMean$limit)),ordered=TRUE) #add observed

actualMean$targets = as.factor(x=actualMean$targets); levels(actualMean$targets) = c("1 target","2 targets", "3 targets")
g=g+geom_point(data=actualMean,size=2) + geom_line(data=actualMean)
g
g=g %+% subset(psychometrics,subject=="mean" & targets==1)

ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")


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
