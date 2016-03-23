#Set locale to USA
Sys.setlocale("LC_ALL", "C")

fp1 = file.path("C:","COURSERA","DATA SCIENCE - STANFORD - Statistical Learning")
fp2 = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - STANFORD - Statistical Learning")

if (dir.exists(fp1)) {
  default_path = fp1
} else if (dir.exists(fp2)) {
  default_path = fp2
} else {
  print('Could not set default path')
}


require(MASS)
require(ISLR)  # datasets from the book

#show column names
names(Boston)

# show help about the boston dataframe
# get info on variables
?Boston

# medv - median value of owner-occupied homes in \$1000s.
# lstat - lower status of the population (percent).

# get summary stats about the data frame
summary(Boston)

# plot median home value vs lower status of pop
# fairly linear correlation
plot(medv~lstat,Boston,col=c('black','blue'))

######
######     Simple linear regression
######

# fit a linear regression model for 1 predictor, lstat
lr1 = lm(medv ~ lstat , data=Boston)

# show summary info for lr1 model
# typically we don't care about the intercept numbers as much

summary(lr1)

# add linear model fit to the plot
abline(lr1,col="red")

#show components of the lr1 model, many are used internally
names(lr1)

# [1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values"
# [6] "assign"        "qr"            "df.residual"   "xlevels"       "call"         
# [11] "terms"         "model"    

#confint finds a confidence interval for a fit
confint(lr1)

#                2.5 %     97.5 %
#   (Intercept)  33.448457 35.6592247
# lstat         -1.026148 -0.8739505

# predict confidence interval for new values
predict(lr1,data.frame(lstat=c(5,10,15)),interval="confidence")


######
######     Multiple linear regression
######

lr2 = lm(medv ~ lstat + age , data=Boston)

# r squared = higher is better, % of variance explained
# f stat = f stat we'd get if we dropped both variables
summary(lr2)

# use all variables
# with all vars, age is not significant
# a lot of other predictors strongly correlated with age
# with all vars included, age is no longer required

lr3 = lm(medv ~ . , data=Boston) ; summary(lr3)

par(mfrow=c(2,2))
plot(lr3) # click zoom if plots are not legible


# update model lr3 by removing a few variables
# nothing on the left side means use same response
lr4 = update(lr3, ~ . -age -indus) ; summary(lr4)

# fit a model with some variable interactions
# * means interaction, not multiplication
lr5 = lm(medv ~ lstat * age , data=Boston) ; summary(lr5)

# use linear and quadriatic lstat as variable
# I() is to protect what's inside of brackets
lr6 = lm(medv ~ lstat + I(lstat^2),data=Boston) ; summary(lr6)

# reset plot space to 1 by 1
par(mfrow=c(1,1))

# plot value of house vs status again
plot(Boston$medv ~ Boston$lstat )

# add fitted values from out quadriatic lr fit (lr6)
# for each value of lstat, it's the value from lr6
# pch = plotting character
points(Boston$lstat,fitted(lr6),col="red",pch=20)


lr7 = lm(medv ~ poly(lstat,4) , data=Boston) ; summary(lr7)
points(Boston$lstat,fitted(lr7),col="blue",pch=20)

# show all pch options
plot(1:20,1:20,pch=1:20,cex=2)

# edit a dataframe on the fly in separate window
fix(Boston)

######
######     Qualitative predictors
######

summary(Carseats)
?Carseats
names(Carseats)

# Income:Advertising appears to be significant, Price:Age is not
lr8 = lm(Sales ~ . + Income*Advertising+Age*Price,data=Carseats) ; summary(lr8)

# for all qualitative variables, 
# use contrasts to show how R will use the var when fitting it
# 3 level factor, R puts in 2 dummy variables
contrasts(Carseats$ShelveLoc)


######
######     Functions in R
######

regplot = function(x,y) {
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}

regplot(Carseats$Price,Carseats$Sales)

# allow for extra args, pass them along to plot
regplot2 = function(x,y,...) {
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}

regplot2(Carseats$Price,Carseats$Sales,
         xlab="Price",ylab="Sales",col="blue",pch=20)

