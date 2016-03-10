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

library(ggplot2)

intl = read.csv(file.path(default_path,"intl.csv"))
str(intl)

ggplot(intl,aes(x=Region,y=PercentOfIntl)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=PercentOfIntl))

#fix x axis being out of order and x axis text overlapping

intl = transform(intl, Region = reorder(Region, -PercentOfIntl))

str(intl)

intl$PercentOfIntl = intl$PercentOfIntl*100

ggplot(intl,aes(x=Region,y=PercentOfIntl)) +
  geom_bar(stat="identity",fill="dark blue") +
  geom_text(aes(label=PercentOfIntl),vjust=-0.4) +
  ylab("Percentage of International Students") +
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=45,hjust=1))


#======================================

intlall = read.csv(file.path(default_path,"intlall.csv"),stringsAsFactors = FALSE)

str(intlall)
head(intlall)

intlall[is.na(intlall)] = 0
head(intlall)

world_map = map_data("world")
str(world_map)

world_map = merge(world_map,intlall,by.x="region",by.y="Citizenship")

ggplot(world_map,aes(x=long,y=lat,group=group)) +
  geom_polygon(fill="white",color="black") +
  coord_map("mercator")

world_map = world_map[order(world_map$group,world_map$order),]

ggplot(world_map,aes(x=long,y=lat,group=group)) +
  geom_polygon(fill="white",color="black") +
  coord_map("mercator")

intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"]  = "China"

intlall$Citizenship

world_map = merge(map_data("world"),intlall,by.x="region",by.y="Citizenship")
world_map = world_map[order(world_map$group,world_map$order),]

ggplot(world_map,aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=Total),color="black") +
  coord_map("mercator")


ggplot(world_map,aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=Total),color="black") +
  coord_map("ortho",orientation = c(20,30,0))

#======================================

#install.packages("reshape2")

library(reshape2)

households = read.csv(file.path(default_path,"households.csv"))

str(households)

#ggplot needs (year,group,fraction)

#first 2 columns of dataframe
households[,1:2]

#each value of marriedwchild turned into its own row, show first 10 rows
melt(households , id="Year")[1:10,]

ggplot(melt(households , id="Year") , aes(x=Year,y=value,color=variable)) +
  geom_line(size=2) +
  geom_point(size=5) +
  ylab("Percentage of Households")





