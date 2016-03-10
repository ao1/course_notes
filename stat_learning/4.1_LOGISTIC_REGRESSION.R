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
require(MASS)
require(ESL)

names(Smarket)
summary(Smarket)

train = subset(Smarket,Smarket$Year < 2005)
test = subset(Smarket,Smarket$Year >= 2005)

######
######     Logistic regression examples on test and train sets
######

fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=train,family="binomial")
probabilities = predict(fit,type="response",newdata=test)
predictions = ifelse(probabilities>0.5,"Up","Down")
table(predictions,test$Direction)
sum(diag(table(predictions,test$Direction))) / sum(table(predictions,test$Direction))
# 48%, we're doing worse than the null rate which is 50%
# we might be overfitting, retry with just the 2 first variables

fit2 = glm(Direction ~ Lag1+Lag2,data=train,family="binomial")
probabilities = predict(fit2,type="response",newdata=test)
predictions = ifelse(probabilities>0.5,"Up","Down")
table(predictions,test$Direction)
sum(diag(table(predictions,test$Direction))) / sum(table(predictions,test$Direction))
# 55.9%, correct classification of 55.9%, smaller model is better


######
######     Logistic regression basics
######

?Smarket

# Year
# The year that the observation was recorded
# 
# Lag1
# Percentage return for previous day
# 
# Lag2
# Percentage return for 2 days previous
# 
# Lag3
# Percentage return for 3 days previous
# 
# Lag4
# Percentage return for 4 days previous
# 
# Lag5
# Percentage return for 5 days previous
# 
# Volume
# Volume of shares traded (number of daily shares traded in billions)
# 
# Today
# Percentage return for today
# 
# Direction
# A factor with levels Down and Up indicating whether the market had a positive or negative return on a given day

#pairwise scatterplot (zoom in to it)
pairs(Smarket,col=Smarket$Direction)

# remember to declare family=binomial for logistic regression
lr1 = glm(Direction ~ Lag1+Lag2++Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(lr1)

# make predictions on training data
prob = predict(lr1,type="response")
prob[1:5]

updown = ifelse(prob>0.5,"Up","Down")

# off diagonals in this table are mistakes
# diagonals are true
table(updown,Smarket$Direction)

# how accurate are our predictions?
sum(diag(table(updown,Smarket$Direction))) / sum(table(updown,Smarket$Direction))
# 52%



######
######     Using the Default dataset, part 1
######

# Which should be the better predictor of Default: Income or Balance?

train = Default[0:8000,]
test = Default[8001:10000,]

logreg_income = glm(default ~ income, data=train,family=binomial)
logreg_balance = glm(default ~ balance, data=train,family=binomial)
logreg_balance_income = glm(default ~ balance+income, data=train,family=binomial)

predict_income_test = predict(logreg_income,type="response",newdata=test)
predict_balance_test = predict(logreg_balance,type="response",newdata=test)

sum(diag(table(test$default,predict_income_test > 0.5))) /
 sum(table(test$default,predict_income_test > 0.5)) * 100
# 96.65

sum(diag(table(test$default,predict_balance_test > 0.5))) /
  sum(table(test$default,predict_balance_test > 0.5)) * 100
# 97.35



summary(logreg_balance)


######
######     Using the Default dataset, part 2
######

logreg = glm(default ~ balance, data=Default,family=binomial)
predict_balance = predict(logreg,type="response",newdata=Default)
summary(logreg)
 
# Coefficients:
#                  Estimate  Std. Error z value            Pr(>|z|)    
# (Intercept)   -10.6513306   0.3611574  -29.49 <0.0000000000000002 ***
#   balance       0.0054989   0.0002204   24.95 <0.0000000000000002 ***

# What is the estimated probability of default for someone with
# a balance of $2000?

exp(-10.6513+0.0055*2000)/(1+exp(-10.6513+0.0055*2000)) 
#0.586

# What value of Balance will give a predicted Default rate of 50%?

# website answer method:
(log(0.5/(1-0.5)) + 10.6513) / 0.0055

#self answer:
exp(-10.6513306)*exp(0.0054989*1936.993) # = p/(1-p) which is 1

10.6513306/0.0054989

#check if it really works
exp(-10.6513306+0.0054989*1936.993) / (1+exp(-10.6513306+0.0054989*1936.993))


######
######     4.3.R1 and 4.3.R2
######

# Suppose we collect data for a group of students in a statistics class with 
# variables X1= hours studied, X2= undergrad GPA, and Y= receive an A. We fit a 
# logistic regression and produce estimated coefficients B0=-6 B1=0.05 B2=1.

# Estimate the probability that a student who studies for 40h and has an 
# undergrad GPA of 3.5 gets an A in the class (within 0.01 accuracy):

B0=-6 ; B1=0.05 ; B2=1 ; hours=40 ; gpa=3.5

# use the log reg probability forumla
exp(B0 + B1*hours + B2*gpa) / (1 + exp(B0 + B1*hours + B2*gpa))
#or
1 / (1 + exp(-(B0 + B1*hours + B2*gpa)))
# 0.3775407


# How many hours would that student need to study to have a 50% chance 
# of getting an A in the class?:

(log(0.50 / (1-0.5))-B0-B2*gpa)/B1
#50


