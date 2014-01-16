#Set working directory to present directory
#dirPrefix should be your own computer's local directory
dirPrefix<-"/Users/alexh/Documents/attention_tempresltn/multiple\ object\ tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/speed-tf-VSS14/"
setwd(paste(dirPrefix,"analyseExps/",sep=''))
load("../data/dat.RData",verbose=TRUE) #Eventually will contain all experiments, including spinzters?
#load("../data/data123targets269objects.RData",verbose=TRUE) 

source('analyzeMakeReadyForPlot.R') #returns fitParms, psychometrics, and function calcPctCorrThisSpeed
#save.image(file="speedLimitsAndTargetLoad.RData") #esp. because might want to analyse this data later as testbed for more robust function fitting

#First, conventional speed limits
source('plotIndividDataWithPsychometricCurves') 

#Based on %corr, but don't have much overlap in speeds in this experiment. 
#Should eventually do it by interpolation in system where take Franconeri's theory literally
#source('anal_costRelativeToWorstCase.R') 

#should also do it normalizing by subjects' speed limits
source("extractThreshesAndPlot.R")

#run codediff on robustify functions