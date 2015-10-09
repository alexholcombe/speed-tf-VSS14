ANOVAreport <- function(aov.out,factorName) { #summarise ANOVA result in pithy sentence
  sm<- summary(aov.out)
  errString<- paste("Error: subject:",factorName,sep='')
  testsAgainstThisErr<- sm[errString]
  testsAgainstThisErr<-testsAgainstThisErr[[1]][[1]]
  #Problem that the rownames of this sometimes have trailing whitespace after the factor
  # returns string w/o trailing whitespace
  trim.trailing <- function (x) sub("\\s+$", "", x)
  rownames(testsAgainstThisErr)<- trim.trailing( rownames(testsAgainstThisErr) )
  F<-testsAgainstThisErr[factorName,"F value"]
  Df<-testsAgainstThisErr[factorName,"Df"]
  Df_error_term<- testsAgainstThisErr["Residuals","Df"]
  p<-testsAgainstThisErr[factorName,"Pr(>F)"]
  msg<- paste(factorName," ",c("","NOT ")[(p>.05)+1],"signif, F(",Df,",",Df_error_term,")=",
              round(F,3),", p=",round(p,6),c("*","")[(p>.05)+1],sep='')
  list( p=p, Df=Df, F=F, msg= msg )
}

# writeLines( ANOVAreport(aov.out,"direction")$msg )
# if (length(unique(thr$separatnDeg))>1) {
#   writeLines( ANOVAreport(aov.out,"separatnDeg")$msg )
#   writeLines( ANOVAreport(aov.out,"separatnDeg:numTargets")$msg )
#   sepDeg<- ANOVAreport(aov.out,"separatnDeg")
#   interaxn<- ANOVAreport(aov.out,"separatnDeg:numTargets")
# }