#Set locale to USA
Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
  
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
  
} else {
  
  print('Could not set default path')
  
}


# http://bconnelly.net/2013/10/creating-colorblind-friendly-figures/
#

who = read.csv(file.path(default_path,"WHO.csv"))

plot(who$GNI , who$FertilityRate)

#install.packages("ggplot2")
library(ggplot2)

#create ggplot2 object
scatterplot = ggplot( who , aes(x=GNI ,  y=FertilityRate))
scatterplot + geom_point()

#each extra command is a layer in ggplot

#switch to line view
scatterplot + geom_line()

#switch back to point view, define some values, shape = figure of point
scatterplot + geom_point( color="blue" , size=3 , shape=17)

#add name on top
scatterplot + geom_point( color="darkred" , size=3 , shape=8) + ggtitle("Fertility Rate vs GNI")

#save to variable (and to file1.pdf)
myplot = scatterplot + geom_point( color="darkred" , size=3 , shape=8) + ggtitle("Fertility Rate vs GNI")
pdf("file1.pdf")
print(myplot)
dev.off()


#start coloring points by 3rd variable
scatterplot2 = ggplot( who , aes(x=GNI ,  y=FertilityRate , color=Region)) + geom_point( size=4 , shape=20 )
scatterplot2

#color by different variable
scatterplot3 = ggplot( who , aes(x=GNI ,  y=FertilityRate , color=LifeExpectancy)) + geom_point( size=4 , shape=20 )
scatterplot3


#ad hoc plots
ggplot( who , aes(x=FertilityRate ,  y=Under15)) + geom_point( size=3 )
#not a linear relationship

ggplot( who , aes(x=log(FertilityRate) ,  y=Under15)) + geom_point( size=3 )

model = lm(Under15 ~ log(FertilityRate) , data=who)

summary(model)

#add regression line layer

ggplot( who , aes(x=log(FertilityRate) ,  y=Under15)) + geom_point( size=3 ) + stat_smooth(method = "lm",level=0.99)

ggplot( who , aes(x=log(FertilityRate) ,  y=Under15)) + geom_point( size=3 ) + stat_smooth(method = "lm",se = FALSE)

ggplot(who,aes(x=log(FertilityRate),y=Under15))+geom_point( size=3 )+stat_smooth(method = "lm",se = FALSE, color="orange")


scatterplot4 = ggplot(who, aes(x = FertilityRate, y = Under15, color=Region)) + geom_point(size=4) + scale_color_brewer(palette="Dark2")
scatterplot4





