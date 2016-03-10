source("http://www.openintro.org/stat/data/present.R")

#dimesions
dim(present)
#variable names
names(present)
#structure
str(present)

present$girls

plot(x = present$year, y = present$girls)
plot(x = present$year, y = present$girls,type="l")
plot(present$year, present$girls+present$boys,type="l")

present$total = present$boys+present$girls
present$boys_prop = present$boys / present$girls

#year with most births, boys and girls
present$year[which.max(present$total)]

#plot proportion of boys relative to girls over time
plot(present$year, present$boys / present$girls, type="l")

#year with largest absolute difference between boys and girls
present$year[which.max(abs(present$boys - present$girls))]






