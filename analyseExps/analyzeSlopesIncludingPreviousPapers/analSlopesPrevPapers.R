#Because this is an analysis of old data, I couldn't use links relative to the default project directory.
#Because have to go to old local data directories.
#dirPrefix should be your own computer's local directory
dirPrefix<-"/Users/alexh/Documents/attention_tempresltn/multiple\ object\ tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/speed-tf-VSS14/analyzeSlopesIncludingPreviousPapers"
setwd(dirPrefix)

#Do it for HolcombeChen2013JoV, and also for HolcombeHoweChenJoV2014
#For TF, and for speed
possibleDataToAnalyze=c("separatnPaper","HolcombeChen2013")
dataToAnalyzeIdx=2
iv<-"speed" #tf
if (possibleDataToAnalyze[dataToAnalyzeIdx]=="separatnPaper") {
  dirPrefix<-"~/Documents/attention_tempresltn/multiple\ object\ tracking/ExperimentsWithWing/speedLimitsAndTargetLoad"
  setwd(paste(dirPrefix,"/allAnalysisForPosting/analyzeSlopesIncludingPreviousPapers/",sep=''))
  #The data file is created by a separate R file that anonymises the raw data
  dirOldData<-"../../../spatialInterference_crowding_and_target_load/dataAnalysisAlexFreshAfterVSS/allAnalysisForPosting/"
  load(paste(dirOldData,"datHolcombeChenSeparatnSpeedLimits.RData",sep=''),verbose=TRUE) 
  iv="speed" #"tf"
  source('psychometricHelpRobust4.R') #load my custom version of binomfit_lims
  setwd("../")
} else if (possibleDataToAnalyze[dataToAnalyzeIdx]=="HolcombeChen2013") {
  load('datHolcombeChen2013.RData') #includes both 2-ring and 3-ring experiment
  dat$numTargets<-dat$numTracked
  dat$exp<-1
}

source('analyzeMakeReadyForPlot.R') #returns fitParms, psychometrics, and function calcPctCorrThisSpeed

#Extract threshes from already-fit psychometric curves.
##############################
#go point by point to find thresholds for each criterion
#Calculate worst maximum %correct for reasonable speed, like 0.1
#For each subject*exp*separatnDeg*numTargets (factorsPlusSubject)
psychometricTemp <- subset(psychometrics,numTargets!="2P")
maxEachConditn<- ddply(psychometricTemp,factorsPlusSubject,function(df)c(pCorr=max(df$pCorr)) )
idx<- which.min(maxEachConditn$pCorr) #Find where performance is worst
worstPcorrMax <- maxEachConditn[idx,"pCorr"]
cat("Worst case (lowest pCorr for lowest",iv,"of",min(psychometricTemp[,iv]),"in psychometrics) is") 
print(maxEachConditn[idx,]) #0.938 for subject UZ 1 target 2 objects
maxCriterion <- .90
threshCriteria<- seq(from=0.70,to=maxCriterion,by=0.05)
threshCriteria<- round(threshCriteria,3) #otherwise, 0.80 is not exactly 0.80!!
scaleCriterion <- function(criterion, numObjects, standardNumObjects) {
  standardChanceRate<- 1.0/standardNumObjects
  proportnUpStd <- (criterion - standardChanceRate) / (1.00 - standardChanceRate)
  equiv<- proportnUpStd*(1.00 - 1.0/numObjects) + 1.0/numObjects
  equiv<- round(equiv,3)
  return (equiv)
}
#standardNumObjects is 2, so set up corresponding function
scaleCriterionThis<-function(criterion,numObjects) { scaleCriterion(criterion,numObjects,2) }

threshCriteria<- seq(from=0.70,to=maxCriterion,by=0.05)
threshCriteria<- round(threshCriteria,3) #Otherwise, 0.8 is not exactly 0.8 !!!!
allCriteria<-threshCriteria
for (numObjects in unique(dat$numObjects)) {
  equivCriteria<- lapply(threshCriteria,scaleCriterionThis,numObjects)
  equivCriteria<- unlist(equivCriteria)
  allCriteria<-c(allCriteria,equivCriteria)
}
allCriteria<-unique(allCriteria)

#Create critCriteria as indicator of what the standard criteria are, so can easily just plot those
critCriterion<-0.8 #In the separation paper I used 80% for 2 objects
if (possibleDataToAnalyze[dataToAnalyzeIdx]=="separatnPaper") {
	stdNumObjects=2
} else stdNumObjects=3

critCriteria<- data.frame(numObjects=stdNumObjects, criterion=critCriterion)
for (numObjects in sort(unique(dat$numObjects))[2:length(unique(dat$numObjects))]) {
  equivCriterion<- scaleCriterion(critCriterion,numObjects,stdNumObjects)
  critCriteria<- rbind(critCriteria, data.frame(numObjects=numObjects,criterion=equivCriterion))
}
critCriteria

threshes <- data.frame()
#For each numObjects condition, I want to look at data at a number of levels. 
#I guess I can think of threshCriteria as for 2 objects, and then create equivalents for 3
for (threshCriterion in allCriteria) {    
    cat('Extracting threshold for criterion:',threshCriterion)
    #use point by point search to find the threshold. 
    myThreshGetNumeric= makeMyThreshGetNumerically(iv,threshCriterion)
    
    psychometricTemp<- psychometrics
    calcThreshForPredictn<- FALSE  #because tracking-two prediction for 6, 9 objects never gets that high. Anyway this is to address
    if (!calcThreshForPredictn)  
      psychometricTemp <- subset(psychometricTemp,numTargets!="2P")
    #Don't do it where criterion corresponds to below chance
    #psychometricTemp <- subset(psychometricTemp, numObjects > 1/threshCriterion) #For these numObjects conditions, chance is above the current criterion    
    threshesThisNumeric = ddply(psychometricTemp,factorsPlusSubject,myThreshGetNumeric) 
    threshesThisNumeric$criterion <- threshCriterion
    threshesThis<- merge(threshesThisNumeric,fitParms)
    threshes<- rbind(threshes, threshesThis)
}
threshes$targets<-threshes$numTargets
#Basically to save across runs with different ivs. threshes_tfSave or threshes_speedSave = threshes
thisThreshesName <- paste("threshes_",iv,"Save",sep='')
assign(thisThreshesName,threshes) 
save(list=thisThreshesName,file=thisThreshesName)
#load("threshes_tfSave",verbose=TRUE)

#source('plotIndividDataAndPsychometrics.R') 

thrCrit<-merge(critCriteria,threshes,by=c("numObjects","criterion"),all=FALSE) 

#should also do it normalizing by subjects' speed limits
source("plotSpeedThreshesAndCost.R")
#An alternative way to do it would be to show the %correct cost of a second target for different 1-target performance levels, as

source("plot_slopes.R")

#Check that error bars standardised in same way for everyone
#ANOVA done same way for everyone.