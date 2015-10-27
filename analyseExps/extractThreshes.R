#Intended to be called by doAllAnalyses.R, which 
#variables expected:
#factorsPlusSubject
#fitParms
#psychometrics
#function calcPctCorrThisIvVal
#iv
#varyLapseRate, lapseMinMax
source('helpers/psychometricHelpRobust6.R') #for makeMyThreshGetNumerically

infoMsg=paste0(iv,"-fit")

lapseMsg=""
if (!varyLapseRate)
  lapseMsg=paste("lapseRate always",unique(lapseMinMax))
#go point by point to find thresholds for each criterion for each factorsPlusSubject
worstLapseRate <- max(fitParms$lapseRate)
paste("Can't calculate threshold above criterion level of",1-worstLapseRate,"because that's the worst subject")
calcMidPointThresh<- TRUE; calcThreeQuartersThresh<- TRUE; calcSixSeventhsThresh<-TRUE
#maxCriterion <- 1-worstLapseRate
maxCriterion <- .95
threshCriteria<- seq(from=.67,to=maxCriterion,by=.06) #high thresholds
threshCriterion = round(threshCriteria,3) #because otherwise can include invisible 10^-21 diff which will trip you up later
threshes <- data.frame()
for (numObjectsThis in unique(fitParms$numObjects)) {
  threshCriteriaThis = threshCriteria
  threshCriteriaNotes = rep("nothingSpecial",length(threshCriteriaThis))
  if (calcMidPointThresh) {
    crit <- 1/numObjectsThis + 0.5*(1-1/numObjectsThis)  #midpoint threshold
    crit<- round(crit,3)
    threshCriteriaThis= c(threshCriteriaThis,crit)
    threshCriteriaNotes = c(threshCriteriaNotes,"midpoint")
  }
  if (calcThreeQuartersThresh) {
    crit <- 1/numObjectsThis + 0.75*(1-1/numObjectsThis)
    crit<- round(crit,3)
    threshCriteriaThis= c(threshCriteriaThis,crit)
    threshCriteriaNotes = c(threshCriteriaNotes,"threeQuarters")    
  }
  if (calcSixSeventhsThresh) {
    crit <- 1/numObjectsThis + 6/7*(1-1/numObjectsThis)
    crit<- round(crit,3)
    threshCriteriaThis= c(threshCriteriaThis,crit)
    threshCriteriaNotes = c(threshCriteriaNotes,"sixSevenths")    
  }    
  for (i in 1:length(threshCriteriaThis)) {
    threshCriterion = threshCriteriaThis[i]
    cat('Extracting thresh for criterion:',threshCriterion)
    #use point by point search to find the threshold. 
    myThreshGetNumeric= makeMyThreshGetNumerically(iv,threshCriterion)
    
    psychometricTemp<- subset(psychometrics,numObjects==numObjectsThis)
    calcThreshForPredictn<- FALSE  #because tracking-two prediction for 6, 9 objects never gets that high. Anyway this is to address
    if (!calcThreshForPredictn)  
      psychometricTemp <- subset(psychometricTemp,numTargets!="2P")
    #Don't do it where criterion corresponds to below chance
    #psychometricTemp <- subset(psychometricTemp, numObjects > 1/threshCriterion) #For these numObjects conditions, chance is above the current criterion
    
    threshesThisNumeric = ddply(psychometricTemp,factorsPlusSubject,myThreshGetNumeric)
    if (threshCriteriaNotes[i] =="threeQuarters") { #look out for where couldn't extract thresh
      whereCouldntExtractThresh <- subset(threshesThisNumeric,is.na(thresh))
      if (length(whereCouldntExtractThresh)>0) {
        lastCouldntExtractThresh<-whereCouldntExtractThresh
      }
    }
    threshesThisNumeric$criterion <- threshCriterion
    threshesThisNumeric$criterionNote <- threshCriteriaNotes[i]
    threshesThis<- merge(threshesThisNumeric,fitParms)
    threshes<- rbind(threshes, threshesThis)
  }
}

threshes$targets<-threshes$numTargets
threshes$distractors<- as.factor(threshes$numObjects-1)
threshes$objects<-as.factor(threshes$numObjects)
