source('helpers/psychometricGgplotHelpRobust3.R') #load my custom version of binomfit_lims
#source('helpers/psychometricHelpRobust6.R') #load my custom version of binomfit_lims

expNum<-1
varyLapseRate = TRUE
#global variables needed by psychometricGgplotHelpRobust.R
if (varyLapseRate) { lapseMinMax= c(0,0.05) }  else  #range of lapseRates to try for best fit
	{ lapseMinMax = c(0.01,0.01) }
chanceRate=.5
factorsForBreakdown = c('exp','numObjects','numTargets')
xLims=c(.04,2.7)  #c(.3,2.5)
yLims=c(.3,1.05)
numPointsForPsychometricCurve=200 
#end global variables expected
verbosity=0 #0-don't print much debugging stuff, 1 prints more, and 2 even more

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
getFitParms <- makeParamFit(lapseMinMax,initialMethod,verbosity=0) #use resulting function for one-shot curvefitting
getFitParmsPrintProgress <- function(df) {  #So I can see which fits yielded a warning, print out what was fitting first.
  cat("Finding best fit (calling fitParms) for")
  for (i in 1:length(factorsPlusSubject) ) #Using a loop print them all on one line
    cat( paste( factorsPlusSubject[i],"=",df[1,factorsPlusSubject[i]])," " )
  cat("\n")
  #print( df[1,factorsPlusSubject] ) #one-line commmand, but breaks across lines
  return( getFitParms(df) )
}
dat$subject <- factor(dat$subject)

#tempDat<- subset(dat,numObjects==2 & numTargets==1 & subject=="AH" ) #Does this well now, using penalized.deviance to compare across lapse rates

fitParms <- ddply(dat, factorsPlusSubject, getFitParmsPrintProgress)
#To-do. Change psychometrics myCurve to accommodate rescaling based on method
#       Stop setting global variables
#     Figure out way to pass method thgough to binomfit_limsAlex

#prediction tracking two if only can track one. myPlotCurve then calculates it.
capacityOneParms <- subset(fitParms, numTargets %in% c(1))
capacityOneParms$numTargets <- "2P"
fitParms<-rbind(fitParms,capacityOneParms)
fitParms$chanceRate <- 1/fitParms$numObjects
#use the fitted parameters to get the actual curves
myPlotCurve <- makeMyPlotCurve4(xLims[1],xLims[2]+.5,numPointsForPsychometricCurve)
psychometrics<-ddply(fitParms,factorsPlusSubject,myPlotCurve)  

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

factorsPlusSubjectSpeed <- factorsPlusSubject
factorsPlusSubjectSpeed[ length(factorsPlusSubjectSpeed)+1 ] <- "speed"
datMeans<- ddply(dat,factorsPlusSubjectSpeed,calcMeans)

calcPctCorrThisSpeed <- function(df,s) {
  #Expects a data.frame for a particular condition (only one row tests this speed, or none and must interpolate)
  thisSpeedIdx<- which(df$speed==s)
  if (length(thisSpeedIdx) > 1) {
    stop('calcPctCorrThisSpeed passed a dataframe with more than one instance of speed s')
  }
  if (length(thisSpeedIdx) ==1) {
    answer<- df$pCorr[thisSpeedIdx] #equivalent to df[thisSpeedIdx,'pCorr']
  } else {  #This speed wasn't tested, so have to interpolate to estimate pCorr
    smallers <- which(df$speed<s)
    if (length(smallers)==0)
      stop(paste('Speed queried,',s,'which is smaller than smallest speed tested,',min(df$speed)))
    closestSmaller<- max( df$speed[smallers] )
    largers <- which(df$speed>s)
    if (length(largers)==0)
      stop(paste('Speed queried,',s,'which is larger than fastest speed tested,',max(df$speed)))
    closestLarger<- min( df$speed[largers] )
    #calculate what fraction of the way s is to the larger
    fractionWayToLarger<- (s-closestSmaller)/(closestLarger-closestSmaller)
    largerPctCorr<- df$pCorr[ which(df$speed==closestLarger) ]
    smallerPctCorr<- df$pCorr[ which(df$speed==closestSmaller) ]
    answer<- smallerPctCorr + fractionWayToLarger*(largerPctCorr-smallerPctCorr)
    #print(paste('closestSmalledfr=',closestSmaller,'closestLarger=',closestLarger))
    #print(paste('fractionWayToLarger=',fractionWayToLarger,'largerPctCorr=',largerPctCorr,'smallerPctCorr=',smallerPctCorr,'answer=',answer))
  }
  return (answer)
}

cat(paste('I give you fitParms, psychometrics, datMeans and function calcPctCorrThisSpeed.'))

#  title<-paste('E',expNum,' individual Ss data',sep='')
#   quartz(title,width=10,height=7)
#   thisExpDat <- subset(dat,exp==expNum)
#   g=ggplot(data= thisExpDat,aes(x=speed,y=correct,color=factor(numTargets),shape=factor(numObjects)))
#   g=g+stat_summary(fun.y=mean,geom="point", position=position_jitter(w=0.04,h=0),alpha=.95)
#   g=g+facet_grid(numObjects ~ subject)+theme_bw()
#   #g<-g+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#   show(g)
#   #draw individual psychometric functions, for only one experiment
#   thisPsychometrics <- subset(psychometrics,exp==expNum)
#   g=g+geom_line(data=thisPsychometrics)
#   g=g+ geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
#   g=g+xlab('Speed (rps)')+ylab('Proportion Correct')
#   #g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
#   #g<- g+ theme(axis.title.y=element_text(size=12,angle=90),axis.text.y=element_text(size=10),axis.title.x=element_text(size=12),axis.text.x=element_text(size=10))
#   g<-g+ scale_x_continuous(breaks=c(0.5,1.0,1.5,2.0,2.5),labels=c("0.5","","1.5","","2.5"))
#   #g<-g+ scale_x_continuous(breaks=speeds)
#   show(g)
