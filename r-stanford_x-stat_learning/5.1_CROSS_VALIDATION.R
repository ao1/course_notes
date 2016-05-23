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

require(ISLR)
require(boot) #need boot package for cross validation

####
####     INVESTIGATE AUTO DATASET
####

?Auto
#Gas mileage, horsepower, and other information for 392 vehicles.

plot(Auto$mpg~Auto$horsepower)
# mpg drops as horespower increases
# data is non-linear


####
####     LINEAR MODEL FIT (USING GLM) + CROSS VALIDATION
####

# fit a linear model using glm
# if you don't pass the family arg, glm fits linear model

glm.fit = glm(mpg~horsepower,data=Auto)

nrow(Auto) # check row count, n = 392

cv.pred = cv.glm(data=Auto,glmfit=glm.fit)
summary(cv.pred)

# cv.glm is a "leave 1 out" cross validation:

# 1. Fits model repeatedly, n times for n observations
#    and each time it leaves out 1 observation. 
#    (It produces the fit on all other data)

# 2. Then it makes a prediction at the x value
#    for the observation it left out.

# Note: cv.glm refits the model all those times 
#       by brute force so it can be slow.

cv.pred$delta # 24.23151 24.23114
# access the delta (cross validated prediction error)

# first number  = the cross validation result, raw result
# second number = bias corrected version of number 1
#                 the dataset we're training on is smaller 
#                 than the full dataset of size n

####
####     WRITE A CUSTOM 'LOOCV' FUNCTION
####

# write our own loocv function to speed up cv.glm

loocv = function(fit)
{
  # lm.influence is a post processor for lm.fit
  h = lm.influence(fit)$h 
  # residuals(fit) is a vector, 1-h is a vector
  mean( (residuals(fit)/(1-h))^2 )
}

loocv(glm.fit) # runs much faster, 24.23151

####
####               FIT POLYNOMIALS
####            OF DIFFERENT DEGREES
####                TO OUR DATA
####


# make a vector of 5 0s, for collecting errors
cv.error = rep(0,5)

# make a vector from 1 to 5, degrees
degrees = seq(1,5,1)

# for each degree:
# 1. fit the glm using the polynomial of that degree
# 2. compute the "leave 1 out" cross validation error

for(degree in degrees){
  glm.fit = glm(mpg~poly(horsepower,degree),data=Auto)
  cv.error[degree] = loocv(glm.fit)
}


plot(degrees,cv.error,type = 'b')
# degree 1 does poorly
# degree 2 (quadriatic) improves a error rate a lot
# degrees 3-5 do not improve error rate


####
####     10-FOLD CROSS VALIDATION
####

# prepare a vector of 5 zeros, for collecting errors
cv.error10 = rep(0,5)

# 1. We divide the data into 10 pieces.
#    Each 1/10 is a tercet. 
#    The 9/10 acts as training set

# 2. Fit the model 10 times.

for(degree in degrees){
  glm.fit = glm(mpg~poly(horsepower,degree),data=Auto)
  cv.error10[degree] = cv.glm(Auto,glm.fit,K=10)$delta[1]
}

lines(degrees,cv.error10,type="b",col="red")
# not much different from "leave 1 out" cross validation
# in general we favor using 10 fold cross validation










