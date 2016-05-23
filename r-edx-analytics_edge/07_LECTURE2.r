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


mvt = read.csv(file.path(default_path,"mvt.csv"),stringsAsFactors = FALSE)

str(mvt)

#======================================

mvt$Date = strptime(mvt$Date , format = "%m/%d/%y %H:%M")

mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

str(mvt)

table(mvt$Weekday)

WeekdayCounts = as.data.frame(table(mvt$Weekday))

str(WeekdayCounts)

#======================================

#install.packages("ggplot2")
library(ggplot2)

ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1))

#make weekday variable in chronological order
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1,ordered = TRUE,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) 

ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1)) + xlab("Day of the week") + ylab("Total Motor Vehicle Thefts")

ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1),linetype=2) + xlab("Day of the week") + ylab("Total Motor Vehicle Thefts")

ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1),alpha=0.3) + xlab("Day of the week") + ylab("Total Motor Vehicle Thefts")


#======================================

table(mvt$Weekday,mvt$Hour)

DayHourCounts = as.data.frame(table(mvt$Weekday,mvt$Hour))

str(DayHourCounts)

DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

#======================================

ggplot(DayHourCounts, aes(x=Hour,y=Freq))+geom_line(aes(group=Var1,color=Var1),size=2)

DayHourCounts$Var1 = factor(DayHourCounts$Var1 , ordered=TRUE , levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot( DayHourCounts , aes(x=Hour,y=Var1)) +
                        geom_tile(aes(fill = Freq)) +
                        scale_fill_gradient(name="Total MV Thefts",low="white",high="red") +
                        theme(axis.title.y = element_blank())



ggplot( DayHourCounts , aes(x=Var1,y=Hour)) +
                        geom_tile(aes(fill = Freq)) +
                        scale_fill_gradient(name="Total MV Thefts",low="white",high="red") +
                        theme(axis.title.y = element_blank())




ggplot( DayHourCounts , aes(x=Hour,y=Var1)) +
                        geom_tile(aes(fill = Freq)) +
                        scale_fill_gradient(name="Total MV Thefts",low="white",high="black") +
                        theme(axis.title.y = element_blank())


#======================================

#install.packages("maps")
#install.packages("ggmap")
  
library(maps)
library(ggmap)
 
#======================================

chicago = get_map(location="chicago",zoom=11)

ggmap(chicago)+geom_point( data=mvt[1:100,],aes(x=Longitude,y=Latitude))

LatLonCounts = as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))

LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var))

ggmap(chicago) + 
  geom_point(data=LatLonCounts,aes(x=Long,y=Lat,color=Freq,size=Freq)) +
  scale_color_gradient(low="yellow",high="red")

ggmap(chicago) + 
  geom_tile(data=LatLonCounts,aes(x=Long,y=Lat,alpha=Freq),fill="red")

#======================================

LatLonCounts2 = subset(LatLonCounts,LatLonCounts$Freq > 0)

ggmap(chicago) + 
  geom_tile(data=LatLonCounts2,aes(x=Long,y=Lat,alpha=Freq),fill="red")

#======================================

murders = read.csv(file.path(default_path,"murders.csv"))

str(murders)

statesMap = map_data("state")
str(statesMap)

ggplot(statesMap,aes(x=long,y=lat,group=group)) +
  geom_polygon(fill="white", color="black")

murders$region = tolower(murders$State)

#======================================

murderMap = merge(statesMap,murders,by="region")


str(murderMap)

ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Murders)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black",high="red",guide="legend")


ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Population)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black",high="red",guide="legend")


murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000

ggplot(murderMap,aes(x=long,y=lat,group=group,fill=MurderRate)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black",high="red",guide="legend")


ggplot(murderMap,aes(x=long,y=lat,group=group,fill=MurderRate)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black",high="red",guide="legend",limits=c(0,10))


ggplot(murderMap,aes(x=long,y=lat,group=group,fill=GunOwnership)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black",high="red",guide="legend")






















