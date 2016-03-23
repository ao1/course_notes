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

###
###   BOOTSTRAPPING
###

# Bootstrapping lets you get at the sampling distribution
# of statistics where it is hard to develop a theoretical
# version

# Handy way of getting estimates of standard error
# for nasty statistics

# Bootstrap does a re-sample of training observations
# some observations can be represented more than once

# Step 1. make function to calculate alpha according
# to formula

alpha = function(x,y)
{
(var(y)-cov(x,y)) / (var(x)+var(y)-2*cov(x,y))
}

alpha(Portfolio$X,Portfolio$Y)
# 0.5758321

# Step 2. make a function to calculate std error of alpha
# - takes dataframe and index
# - computes the alpha index
# - index has values 1 to n
# - draw with replacement

alpha.fn = function(data,index)
{
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:100)
# 0.5758321

set.seed(1)


# Step 3.
# Boostrap will do this over and over
alpha.fn(Portfolio,sample(1:100,100,replace = T))

# do 1000 bootstraps
boot.out = boot(Portfolio , alpha.fn , R=1000)

# summary of bootstrap
boot.out

# Bootstrap Statistics :
# original       bias           std. error
# t1* 0.5758321  -7.315422e-05  0.08861826

# the bias is negligible

# if it lines up on a straight line, it is gaussian
plot(boot.out)

# symmetric distribution
# qq plot is close to gaussian
# reliable way to get estimates of std error 
# for hard statistics



