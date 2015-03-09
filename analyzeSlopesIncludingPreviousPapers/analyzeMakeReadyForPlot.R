source('psychometricHelpRobust4.R') #load my custom version of binomfit_lims
dat$tf <- dat$speed * dat$numObjects

varyLapseRate = TRUE

errBarUpper <- function(x) { 
  x.mean <- mean(x) 
  x.sd <- sd(x) 
  SEM <- x.sd / (sqrt(length(x))) 
  return(x.mean + (SEM)) 
} 
errBarLower <- function(x) { 
  x.mean <- mean(x) 
  x.sd <- sd(x) 
  SEM <- x.sd / (sqrt(length(x))) 
  return(x.mean - (SEM)) 
} 

#global variables needed by psychometricFitGgplotHelpers.R
if (varyLapseRate) { lapseMinMax= c(0,0.06) }  else  #range of lapseRates to try for best fit
{ lapseMinMax = c(0.01,0.01) }
chanceRate=.5
factorsForBreakdown = c('exp','numObjects','numTargets','numRings')
xLims=c(.1,3.5); if (iv=="tf") {xLims=c(.5,8)}
yLims=c(.3,1.05)
numPointsForPsychometricCurve=200 
linkf="logit"  #"probit"  #must be either 'logit' or 'probit'; this is the linking function used in the GLM in the first instance. If it fails, system will switch to other
#end global variables expected
verbosity=0 #0-don't print much debugging stuff, 1 prints more, and 2 even more

colrFactr = paste('factor(',factorsForBreakdown[1],')',sep='')
facetCols='subject' #'.'
facetRows='.'
if (length(factorsForBreakdown)>1)
  facetRows = factorsForBreakdown[2]
faceting=paste('~',facetCols,sep='')
factorsPlusSubject<-factorsForBreakdown
factorsPlusSubject[ length(factorsForBreakdown)+1 ]<- "subject"

#fit psychometric functions to data #######################################################
initialMethod<- "brglm.fit" #"glmCustomlink"
getFitParms <- makeParamFit(iv,lapseMinMax,initialMethod,verbosity) #use resulting function for one-shot curvefitting
getFitParmsPrintProgress <- function(df) {  #So I can see which fits yielded a warning, print out what was fitting first.
  cat("Finding best fit (calling fitParms) for")
  for (i in 1:length(factorsPlusSubject) ) #Using a loop print them all on one line
    cat( paste( factorsPlusSubject[i],"=",df[1,factorsPlusSubject[i]])," " )
  cat("\n")
  #print( df[1,factorsPlusSubject] ) #one-line commmand, but breaks across lines
  return( getFitParms(df) )
}
dat$subject <- factor(dat$subject)

#dat<- subset(dat,numObjects==2 & numTargets==1 & subject=="AH" ) #debugON
fitParms <- ddply(  dat, factorsPlusSubject, getFitParmsPrintProgress ) 

#prediction tracking two if only can track one. myPlotCurve then calculates it.

capacityOneParms <- subset(fitParms, numTargets %in% c(1))
capacityOneParms$numTargets <- "2P"
fitParms<-rbind(fitParms,capacityOneParms)
fitParms$chanceRate <- 1/fitParms$numObjects
#use the fitted parameters to get the actual curves
myPlotCurve <- makeMyPlotCurve4(iv,xLims[1],xLims[2]+.5,numPointsForPsychometricCurve)
psychometrics<-ddply(fitParms,factorsPlusSubject,myPlotCurve)  
##################################################################
#Usually ggplot with stat_summary will collapse the data into means, but for some plots and analyses can't do it that way.
#Therefore calculate the means
calcMeans<-function(df) {
  if ( !("correct" %in% names(df)) )
    warning("your dataframe must have a column named 'correct'",immediate.=TRUE)
  numCorrect<-sum(df$correct==1)
  numTrials= sum(complete.cases(df$correct))
  pCorr <- numCorrect/numTrials
  df= data.frame(pCorr)
  return(df)
}  

calcPctCorrThisIvVal <- function(df,iv,val) {
  #Expects a data.frame for a particular condition (only one row tests this speed, or none and must interpolate)
  thisValIdx<- which(df[,iv]==val)
  if (length(thisValIdx) > 1) {
    stop('calcPctCorrThisSpeed passed a dataframe with more than one instance of speed s')
  }
  if (length(thisValIdx) ==1) {
    answer<- df$pCorr[thisValIdx] #equivalent to df[thisSpeedIdx,'pCorr']
  } else {  #This speed wasn't tested, so have to interpolate to estimate pCorr
    smallers <- which(df[,iv]<val)
    if (length(smallers)==0)
      stop(paste('IV val queried,',val,' is smaller than smallest val tested,',min(df[,iv])))
    closestSmaller<- max( df[smallers,iv] )
    largers <- which(df[,iv]>val)
    if (length(largers)==0)
      stop(paste('IV val queried,',val,' is larger than largest val tested,',max(df[,iv])))
    closestLarger<- min( df[largers,iv] )
    #calculate what fraction of the way s is to the larger
    fractionWayToLarger<- (val-closestSmaller)/(closestLarger-closestSmaller)
    largerPctCorr<- df$pCorr[ which(df[,iv]==closestLarger) ]
    smallerPctCorr<- df$pCorr[ which(df[,iv]==closestSmaller) ]
    answer<- smallerPctCorr + fractionWayToLarger*(largerPctCorr-smallerPctCorr)
    #print(paste('closestSmalledfr=',closestSmaller,'closestLarger=',closestLarger))
    #print(paste('fractionWayToLarger=',fractionWayToLarger,'largerPctCorr=',largerPctCorr,'smallerPctCorr=',smallerPctCorr,'answer=',answer))
  }
return (answer)
}

factorsPlusSubjectAndIv <- factorsPlusSubject
factorsPlusSubjectAndIv[ length(factorsPlusSubjectAndIv)+1 ] <- iv
datMeans<- ddply(dat,factorsPlusSubjectAndIv,calcMeans)

cat(paste('With iv=',iv,'I give you fitParms, psychometrics, datMeans and function calcPctCorrThisSpeed.'))
if (!exists("fitParms") | !exists("psychometrics") | !exists("datMeans") | 
    !exists("calcPctCorrThisIvVal"))
  print("Actually WAIT! one or more of those didn't get made")