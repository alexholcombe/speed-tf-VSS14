p <- ggplot(mtcars, aes(wt, mpg, color=factor(gear), size = factor(gear))) 
p<-p+ geom_point() +theme_classic()
rectLims<-data.frame(xmin=5, xmax=Inf, ymin=-Inf, ymax=Inf, gear=3,mpg=3,cyl=3,wt=2)
p<-p+geom_rect(data=rectLims, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey50",alpha=0.5)
#Hack a manual legend for the superposed rectangles highlighting different parts of the graphs
p

p+  theme(legend.key = element_blank() #, #don't put boxes around legend bits
        #legend.background= element_rect(fill="transparent",color="grey90"), #put big light grey box around entire legend
  )
p+  theme(legend.key = element_rect(fill="grey40")) #, #don't put boxes around legend bits

p
#I need to make the symbols big, only then can have fill?  Fillable shape
p+ guides(fill = guide_legend(title="limitation",override.aes=aes(fill="red",color="green",shape=1))) 
p+ guides(fill = guide_legend(override.aes= list( fill=c("red","red","red"))))

#http://stackoverflow.com/questions/29994535/how-to-selectively-add-box-around-legend-key
#Find the grob for individual legend background
# get ggplot grob
library(gtable)

gt = ggplotGrob(p)
# Get the combined legend
leg = gtable_filter(gt, "guide-box")

# The legend has two parts.
# Get the second part - the color legend
leg2 = leg$grobs[[1]]$grobs[[2]]

# Get the locations of the top of each box containing the legend keys
# in this legend's layout
rects <- leg2$layout$t[grepl("bg", leg2$layout$name)]

# Draw boxes around each key
for(i in rects) leg2 = gtable_add_grob(leg2, grid.rect(gp = gpar(col = '#bdbdbd', fill = NA)), t = i, l = 2)


# Insert new color legend back into the combined legend   
leg$grobs[[1]]$grobs[2][[1]] <- leg2

# Insert combined legend back into ggplot grob
gt$grobs[gt$layout$name == "guide-box"][[1]] <- leg

# Draw it
grid.newpage()
grid.draw(gt)

packageVersion("ggplot2")
