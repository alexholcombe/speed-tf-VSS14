#Intended to be called by doAllAnalyses.R in same directory, which 
#variables expected:
#factorsPlusSubject
#fitParms
#psychometrics
#function calcPctCorrThisIvVal
#iv
#varyLapseRate, lapseMinMax
#threshes from extractThreshesAndPlot

#For each subject, calculate variance of thresh around mean speed vs. mean tf
unique(threshes$criterion)
#Calculate mean thresh across numObjects, and variance around that
criterion = 
#instead of midpoint threshes I should probably use 3/4 threshes. Should probably make new dv
#called midpoint thresh and 3/4 thresh rather than have to recalculate without knowing if that particular criterion is there

varianceCalc <- function(df, iv) {
  criterionToUse = 0.85 #eventually change to 3/4 or something
  thrThisCrit = subset(df, criterion==criterionToUse)
  print(thrThisCrit$thresh)
  sd(thrThisCrit$thresh)
}

varEachSubjTargetsSpeed = ddply(threshes,.(subject,targets),varianceCalc,"speed")
