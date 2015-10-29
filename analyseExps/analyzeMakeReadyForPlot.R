#expects iv, dat
source('helpers/psychometricHelpRobust6.R') #load my custom version of binomfit_lims

varyLapseRate = FALSE
#global variables needed by psychometricGgplotHelpRobust.R
if (varyLapseRate) { lapseMinMax= c(0,0.05) }  else  #range of lapseRates to try for best fit
	{ lapseMinMax = c(0.01,0.01) }
chanceRate=.5
factorsForBreakdown = c('exp','numObjects','numTargets')
xLims=c(.018,2.6);  
if (iv=="logSpd") {xLims= log(xLims)} #c(-2.5,1.2)}
if (iv=="log10spd") {xLims= log10(xLims)} #c(-2.5,1.2)}
if (iv=="tf") {xLims=c(.1,8)} 
yLims=c(.3,1.05)
numPointsForPsychometricCurve=150 #250
#end global variables expected
verbosity=-1 #-1 don't print progress, 0-don't print much debugging stuff, 1 prints more, and 2 even more

colrFactr = paste('factor(',factorsForBreakdown[1],')',sep='')
if ( length(factorsForBreakdown)>1 ) 
  shapeFactr = paste('factor(',factorsForBreakdown[2],')',sep='') else #can't break line before else
  { shapeFactr = colrFactr }
facetCols='subject' #'.'
facetRows='.'
if (length(factorsForBreakdown)>1)
  facetRows = factorsForBreakdown[2]
faceting=paste('~',facetCols,sep='')
factorsPlusSubject<-factorsForBreakdown
factorsPlusSubject[ length(factorsForBreakdown)+1 ]<- "subject"

#fit psychometric functions to data ########################################
initialMethod<-"brglm.fit"  # "glmCustomlink" #  
getFitParms <- makeParamFit(iv,lapseMinMax,initialMethod,verbosity) #use resulting function for one-shot curvefitting
getFitParmsPrintProgress <- function(df) {  #So I can see which fits yielded a warning, print out what was fitting first.
  if (verbosity > -1) {
    cat("Finding best fit (calling fitParms) for")
    for (i in 1:length(factorsPlusSubject) ) #Using a loop print them all on one line
      cat( paste( factorsPlusSubject[i],"=",df[1,factorsPlusSubject[i]])," " )
    cat("\n")
  }
  #print( df[1,factorsPlusSubject] ) #one-line commmand, but breaks across lines
  return( getFitParms(df) )
}
dat$subject <- factor(dat$subject)

#tempDat<- subset(dat,numObjects==2 & numTargets==1 & subject=="AH" ) #Does this well now, using penalized.deviance to compare across lapse rates
fitParms <- ddply(dat, factorsPlusSubject, getFitParmsPrintProgress)
cat("Mean deviance with iv",iv,"=",mean(fitParms$deviance))
#To-do. Change psychometrics myCurve to accommodate rescaling based on method
#       Stop setting global variables
#     Figure out way to pass method thgough to binomfit_limsAlex

#Could use quickpsy, but can't handle different guessing rates in single call.
#So I'd have to call it separately for each numObjects and then modify plotcurves
#to get it to plot them all.
#Also I suspect I'd need to write functions to get thresholds at different levels
useQuickpsy<-FALSE
if (useQuickpsy) {
  library(devtools) #So can load from Alex's local quickpsy package repository
  #install_github('danilinares/quickpsy')
  load_all("/Users/alexh/Documents/softwareStatsEquipment/programming_psychophysics/quickpsy/quickpsy")
  #library('quickpsy')
  #Create decreasing function to fit
  negCumNormal<-function(x,p) { cum_normal_fun(-x,p) }
  #factorsPlusSubject "exp"        "numObjects" "numTargets" "subject" 
  datDani<-dat  
  datDani$speed = -1*datDani$speed
  #craps out because probabilites not within 0 and 1
  fitCondSubj <- quickpsy(datDani, speed, correct, 
                          grouping=.(exp,subject,numTargets,numObjects), #factorsPlusSubject,
                          bootstrap='none', xmin=.1, xmax=2.2,
                          guess=TRUE #estimate chance performance, although weird
                          )
  #craps out because "Error in optim(parini, nllfun) : function cannot be evaluated at initial parameters
  fitCondSubj <- quickpsy(dat, speed, correct, 
                          grouping=.(exp,subject), #factorsPlusSubject,
                          bootstrap='none', xmin=.1, xmax=2.2,
                          fun=negCumNormal, 
                          guess=TRUE, #estimate chance performance, although weird
                          parini = c(1.5,10))
  plot1 <- plotcurves(fitCondSubj) + theme_bw()
  quartz(tit); show(plot1)
}

#prediction tracking two if only can track one. myPlotCurve then calculates it.
#use the fitted parameters to get the actual curves
myPlotCurve <- makeMyPlotCurve4(iv,xLims[1],xLims[2]+.5,numPointsForPsychometricCurve)
#ddply(fitParms,factorsPlusSubject,function(df) { if (nrow(df)>1) {print(df); STOP} })  #debugOFF
psychometrics<-ddply(fitParms,factorsPlusSubject,myPlotCurve)
if (iv=="logSpd") {
  psychometrics$speed = exp(1)^psychometrics$logSpd
}
if (iv=="log10spd") {
  psychometrics$speed = 10^psychometrics$log10spd
}
if (!("tf" %in% colnames(psychometrics))) { #psychometrics must have been fit to tf
  psychometrics$tf <- psychometrics$speed * psychometrics$numObjects
}
if (!("speed" %in% colnames(psychometrics))) { #psychometrics must have been fit to tf
  stopifnot("tf" %in% colnames(psychometrics)) #confirm my interpretation that tf was fit
  psychometrics$speed = psychometrics$tf / psychometrics$numObjects #so can plot them in terms of speed
}

#Below are just helper functions. Consider migration into a helper function file
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
factorsPlusSubjectAndIv <- factorsPlusSubject
factorsPlusSubjectAndIv[ length(factorsPlusSubjectAndIv)+1 ] <- iv
datMeans<- ddply(dat,factorsPlusSubjectAndIv,calcMeans)

calcPctCorrThisIvVal <- function(df,iv,val) {
  #Take dataframe with fitted psychometric function, 
  #where only one row tests this iv val, or none and must interpolate
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

cat(paste(' I give you fitParms, psychometrics, datMeans and function calcPctCorrThisIvVal.'))
stopifnot(exists("fitParms"))
stopifnot(exists("psychometrics"))
stopifnot(exists("datMeans"))
stopifnot(exists("calcPctCorrThisIvVal"))
