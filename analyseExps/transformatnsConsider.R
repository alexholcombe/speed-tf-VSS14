#To address the thresholds that turn up negative if HC2013 fit to speed, 
#what transformation should be used?

basic<-data.frame(x=seq(.05,2,length.out=300))
basic$y = log(basic$x)
basic$base<-"e"
df<-basic

basic$y = log2(basic$x)
basic$base<- "2"
df<-rbind(df,basic)

basic$y = log10(basic$x)
basic$base<- "10"
df<-rbind(df,basic)

# basic$y = sqrt(basic$x)
# basic$base<- "sqrt"
# df<-rbind(df,basic)

basic$y = -1/basic$x
basic$base<- "-1/x"
df<-rbind(df,basic)

basic$y = -1/(basic$x^.5)
basic$base<- "-1/(x^.5)"
df<-rbind(df,basic)

basic$y = -1/(basic$x^.3)
basic$base<- "-1/(x^.3)"
df<-rbind(df,basic)

require(ggplot2)
g<- ggplot(df, aes(x=x,y=y,color=base) )
g<-g + geom_point()
show(g)

#They differ by a multiplicative factor
subset(df,base=="2")$y / subset(df,base=="10")$y 
#So they are linear transformation of each other, does that mean that all logs
#will fit equally well? YES