#Set locale to USA
Sys.setlocale("LC_ALL", "C")


if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
  
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
  
} else {
  
  print('Could not set default path')
  
}


boston = read.csv(file.path(default_path,"boston.csv"))

str(boston)

plot(boston$LON,boston$LAT)

points( boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue", pch=19 )

points( boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red", pch=19 )

summary(boston$NOX)

points( boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],col="green", pch=19 )

#reset plot
plot(boston$LON,boston$LAT)
summary(boston$MEDV)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00   17.02   21.20   22.53   25.00   50.00 

#add points, all census tracts with above avg prices
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)



plot(boston$LON,boston$MEDV)
plot(boston$LAT,boston$MEDV)

#linear regression model for 
latlonlm = lm(MEDV ~ LAT + LON,data=boston)

summary(latlonlm)
# LAT is not significant, north/south arent of use
# r squared is 0.1 which is not great 
# as we go towardsthe oceans, house prices decrease, 
# seems unlikely at first glance

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)

#regression model predictions for each census tract
latlonlm$fitted.values

#blue dollar signs
points(boston$LON[latlonlm$fitted.values >= 21.2],boston$LAT[latlonlm$fitted.values >= 21.2],col="blue",pch="$")

#regression trees
library(rpart)
library(rpart.plot)

latlontree = rpart(MEDV ~ LAT + LON , data=boston)
prp(latlontree)
#lot of splits, leafs = buckets

plot(boston$LON,boston$LAT)
#above median price points
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)

fittedvalues = predict(latlontree)

points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="blue",pch="$")

#make a simpler tree

latlontree = rpart(MEDV ~ LAT + LON , data=boston,minbucket=50)


plot(latlontree)
text(latlontree)

plot(boston$LON,boston$LAT)

#vertical line
abline(v=-71.07)
#horizontal line
abline(h=42.21)
abline(h=42.17)

points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)



library(caTools)

set.seed(123)

split = sample.split(boston$MEDV,SplitRatio=-0.7)

train = subset(boston,split==TRUE)
test = subset(boston,split==FALSE)


linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO , data=train)


summary(linreg)

#some of these might be corelated, so cant trust the signif. codes completely


linreg.pred = predict(linreg, newdata=test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
#3037.088



tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO , data=train)

#RM most important split, crime, age, noxious, non-linear on number of rooms
prp(tree)

tree.pred = predict(tree,newdata=test)
tree.ssd = sum((tree.pred-test$MEDV)^2)

#4328.988, higher than liner reg

#trees are not as good as linear reg for this problem



# cp = complexity parameter, small number = large trees
# too many splits = bad for generalization
# rss residual sum of squares, or ssd


#cross validation

library(caret)
library(e1071)

tr.control = trainControl(method="cv",number=10)

cp.grid = expand.grid( .cp = (0:10)*0.001  )


tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart" , trControl = tr.control , tuneGrid = cp.grid)

#The final value used for the model was cp = 0.001.
#best RM/SE 

best.tree = tr$finalModel

#more detailed tree, has LAT and LON as well
prp(best.tree)

#test

best.tree.pred = predict(best.tree,newdata=test)
best.tree.sse = sum((best.tree.pred-test$MEDV)^2)

best.tree.sse
#3675.766
#not as good as linreg model

linreg.see = 3030



















