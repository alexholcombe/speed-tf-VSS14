#This is a stand-alone file for making plots of speed-tf tracking region
#working directory set by starting Rstudio via .Rproj file
#setwd("/Users/alexh/Documents/attention_tempresltn/multiple object tracking/ExperimentsWithWing/speedLimitsAndTargetLoad/allAnalysisForPosting/speed-tf-VSS14")

source('helpers/psychometricHelpRobust6.R') #for makeMyPsychoCorr, 

tfLimit1_2_3targets = c(7,4.2,2.5) #based on VSS2014 poster midpoint thresholds
speedLimit1_2_3targets = c(2.1,1.9,1.2) #from VSS2014 poster 1-distractor eyeballed thresholds
intersectionPoint = c(-99,-99,-99)
#the window of tracking
tfLimitCalc<-function(targetNum,distractorNum) {
  tfLimit1_2_3targets[targetNum] / (distractorNum+1)
}
#winT, the windowOfTracking
winT = expand.grid( distractors = seq(1,11,1), targets = seq(1,3,1), limit=c("speed","tf") )
winT$thresh=-1
for (target in c(1,2,3)) {
  distractrs = winT[ winT$targets==target & winT$limit=="tf", ]$distractors  
  winT[ winT$targets==target & winT$limit=="tf", ]$thresh = tfLimitCalc(target,distractrs)
  winT[ winT$targets==target & winT$limit=="speed", ]$thresh = speedLimit1_2_3targets[target]
  #intersection is where tfLimit1_2_3targets[target] / (distractrs+1) = speedLimit
  # distractrs + 1 = tfLimit1_2_3targets[target] / speedLimit 
  # distractrs = tfLimit1_2_3targets[target] / speedLimit  -1
  intersectionPoint[target] = tfLimit1_2_3targets[target] / speedLimit1_2_3targets[target]  -1
}

#calculate speed threshold corresponding to lower limit for this num distractors
constrainingLimit<-function(targetNum,distractorNum) {
  if (distractorNum <= intersectionPoint[targetNum]) {
    return (speedLimit1_2_3targets[targetNum])
  }
  else { return (tfLimitCalc(targetNum,distractorNum)) }
}
#create shading inside intersection of limits. 
uniqDistractrs = unique(winT$distractors)
tfLimitedDistractrs1target = uniqDistractrs[ uniqDistractrs>intersectionPoint[1] ]
polygonXs = c(min(uniqDistractrs),intersectionPoint[1], tfLimitedDistractrs1target, #top edge of polygon
              max(tfLimitedDistractrs1target), min(uniqDistractrs)) #bottom edge of polygon
ysPolygonTop = unlist( lapply(polygonXs[1:(length(polygonXs)-2)],FUN=constrainingLimit,targetNum=1) )

positions <- data.frame( #vertices of the polygon
  targets = rep(c(1), each = length(polygonXs)), #targetsID
  limit = "tf",
  x= polygonXs,
  y= c(ysPolygonTop,0,0)
)

#Show 1-target limit only. As the first figure explaining the two limits and their intersection
tit="windowOfTracking 1target"
quartz(tit,width=6.4,height=3.5)
g=ggplot(subset(winT,targets==1), 
         aes( x=distractors,y=thresh, color=factor(targets), linetype=factor(limit)) )
g=g+geom_line(size=.75)
g=g+scale_linetype_manual(values=c(2,3)) #make them both dashed, then make solid the lowest limit
#g=g+scale_linetype_manual(values=c(2,2)) #make them both dashed, then make solid the lowest limit
g=g+coord_cartesian(xlim=c(min(winT$distractors),max(winT$distractors)),ylim=c(0,max(winT$thresh)+.1))
g=g+geom_polygon(data = positions, aes(x, y, targets), fill="pink", color="transparent", alpha=.7)
g=g+annotate("text", x=6,y=1, label=paste(toString(tfLimit1_2_3targets[1]),"Hz"), angle=-15 )
g=g+annotate("text", x=4,y=speedLimit1_2_3targets[1], 
             label=paste(toString(speedLimit1_2_3targets[1]),"rps") )
g=g+annotate("text", x=2.5, y=1, label="trackable", fontface=3, alpha=.8) #italics
g=g+ylab('speed threshold (rps)')
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
#g=g+facet_grid(targets~.) #facet_grid(targets~criterion)
show(g)

#Show 1, 2, and 3 targets trackable regions. As Russian dolls
tit="windowOfTracking 1target"
quartz(tit,width=6.4,height=3.5)
g=ggplot(subset(winT,targets==1), 
         aes( x=distractors,y=thresh, color=factor(targets), linetype=factor(limit)) )
g=g+geom_line(size=.75)
g=g+scale_linetype_manual(values=c(2,3)) #make them both dashed, then make solid the lowest limit
#g=g+scale_linetype_manual(values=c(2,2)) #make them both dashed, then make solid the lowest limit
g=g+coord_cartesian(xlim=c(min(winT$distractors),max(winT$distractors)),ylim=c(0,max(winT$thresh)+.1))
g=g+geom_polygon(data = positions, aes(x, y, targets), fill="pink", color="transparent", alpha=.7)
g=g+annotate("text", x=6,y=1, label=paste(toString(tfLimit1_2_3targets[1]),"Hz"), angle=-15 )
g=g+annotate("text", x=4,y=speedLimit1_2_3targets[1], 
             label=paste(toString(speedLimit1_2_3targets[1]),"rps") )
g=g+annotate("text", x=2.5, y=1, label="trackable", fontface=3, alpha=.8) #italics
g=g+ylab('speed threshold (rps)')
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
#g=g+facet_grid(targets~.) #facet_grid(targets~criterion)
show(g)



#graph speed, tf limits, 
tit="windowOfTracking"
quartz(tit,width=6.4,height=3.5)
g=ggplot(winT, aes( x=distractors,y=thresh, color=factor(targets), linetype=factor(limit)) )
#g=g+geom_point()
g=g+geom_line(size=.75)
g=g+scale_linetype_manual(values=c(2,2)) #make them both dashed, then make solid the lowest limit
g=g+coord_cartesian(xlim=c(min(winT$distractors),max(winT$distractors)),ylim=c(0,max(winT$thresh)+.1))
g=g+geom_polygon(data = positions, aes(x, y, targets), fill="pink", color="transparent", alpha=.7)
g=g+annotate("text", x=6,y=1, label=paste(toString(6.5),"Hz"), angle=-15 )
g=g+ylab('speed threshold (rps)')
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
#g=g+facet_grid(targets~.) #facet_grid(targets~criterion)
show(g)

#Rather than having one linetype for speed limit and one for tf limit, have the polygon enclosing
#the lower limit always solid, and the non-constraining limit dashed or dotted.
g=g+ geom_line(data=positions[1:(nrow(positions)-2),],aes(x,y-.03),linetype=1,size=1.5,alpha=.9)
show(g)



  
  y= constrainingLimit(targets,polygonXs,)
  y= c(speedLimit1_2_3targets[1],speedLimit1_2_3targets[1],0,0, )
  
  
  x = c(0,intersectionPoint[1],intersectionPoint[1],0),
  y = c(speedLimit1_2_3targets[1],speedLimit1_2_3targets[1],0,0) )

positions <- data.frame(
  targets = rep(c(1), each = 4), #targetsID
  limit = "tf",
  x = c(0,intersectionPoint[1],intersectionPoint[1],0),
  y = c(speedLimit1_2_3targets[1],speedLimit1_2_3targets[1],0,0) )



positions<- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

g=g+geom_polygon(data = shade, aes(x, y))

subset(winT,distractors > intersection)
shade <- rbind(c(0.12,0), subset(MyDF, x > 0.12), c(MyDF[nrow(MyDF), "X"], 0))


#Then use this new data.frame with geom_polygon
p + geom_segment(aes(x=0.12,y=0,xend=0.12,yend=ytop)) +
  geom_polygon(data = shade, aes(x, y))

tit="limits"
quartz(tit,width=6.4,height=3.5)
# lims = rbind(  data.frame(limit="tf",val=tfLimit1_2_3targets ), 
#                data.frame(limit="speed",val=speedLimit1_2_3targets )  )
lims = data.frame(tf=tfLimit1_2_3targets, speed=speedLimit1_2_3targets )
lims$targets = c(1,2,3)
g=ggplot(lims,aes(x=tf,y=speed,color=factor(targets))) + geom_point()
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
show(g)

  winT, aes( x=distractors,y=thresh, color=factor(targets), linetype=factor(limit)) ) 
g=g+geom_line(size=1.5)
g=g+annotate("text", x=6,y=1, label=paste(toString(tf),"Hz"), angle=-15 )
g=g+ylab('speed threshold (rps)')
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
#g=g+facet_grid(targets~.) #facet_grid(targets~criterion)
show(g)

# tit="2limits"
# quartz(tit,width=6.4,height=3.5)
# g=ggplot(tDf, aes( x=distractors,y=tfLimit)) 
# g=g+geom_line()
# g=g+annotate("text", x=6,y=1, label=paste(toString(tf),"Hz"), angle=-15 )
# g=g+ylab('speed threshold (rps)')
# g=g+themeAxisTitleSpaceNoGridLinesLegendBox
# g=g+geom_hline(aes(yintercept=speedLimit))
# show(g)
#          
# g=g+ facet_grid(targets~.) #facet_grid(targets~criterion)
# dodgeAmt=0.2
# g=g+ylab('threshold speed (rps)')
# g=g+geom_point(size=2.5,position=position_dodge(width=dodgeAmt))
# combindOnly=subset(threshes,limit=="combined")
# g=g+geom_line(data=combindOnly,aes(group=subject),position=position_dodge(dodgeAmt))
# #g=g+geom_line() #doesn't work
# g=g+scale_alpha_manual(values=c(.6,.6,1)) #combined thickest
# g=g+scale_color_manual(values=c("blue","red","black")) #make combined black

#################################################################################
#Multiply it with each other, then extract resulting threshes
pAfterBothLimits<- function(p1,p2,numObjects,lapse) {
  #Take guesses out (assumption: high-threshold model) of each p
  #to yield t, the "true function"- probability of successful tracking.
  #Otherwise, have to guess.
  #Then multiply probs together and re-insert guessing rate
  l = lapse
  c = 1/numObjects #chanceRate
  #Derived the below by taking standard psychometric function
  # p = l*c + (1-l)(t + (1-t)*c) #and solving for t
  t1 = (p1 - c) / (1 + l*c - l - c)
  t2 = (p2 - c) / (1 + l*c - l - c)
  #probability don't fall afoul of either limit
  b = t1*t2
  #re-insert lapse rate and guessing
  pAfter = l*c + (1-l)*(b + (1-b)*c)
  #cat('p1=',p1,' p2=',p2,' b=',b, ' pAfter=',pAfter)
  #cat('t1=',t1,' t2=',t2,' b=',b, ' pAfter=',pAfter)
  as.numeric(pAfter)
}



