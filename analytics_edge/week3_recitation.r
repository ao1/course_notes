#Set locale to USA
Sys.setlocale("LC_ALL", "C")

default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")

polling = read.csv(file.path(default_path,"PollingData.csv"))

str(polling)

table(polling$Year)

# 2004 2008 2012 
# 50   50   45 

#in 2012 poll people were so sure about 5 states, that they did not poll them

summary(polling)

#Rasmussen and SurveyUSA have missing data (46 and 71)

# Options for dealing with missing data:
# 
# - Delete missing observations -> No
# - Delete variables with missing values -> No
# - Fill missing data points with average values -> The average value for a poll will be close to 0 (tie)
# - Fill missing data points based on non-missing values -> Use the Multiple Imputation by Chained Equations (mice) package

#install.packages("mice")

library(mice)

#create 'simple' polling data frame limited to 4 variables
simple = polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]

summary(simple)

#set random seed
set.seed(144)

#5 rounds of imputation will run, filling all NA values
imputed = complete(mice(simple))

#copy Rasmussen and SurveyUSA values back into the original dataframe variables
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA  

summary(polling)

#make a training and test set, training for 2004 and 2008 data, test for 2012 data
train = subset(polling,Year == 2004 | Year == 2008)
test = subset(polling,Year == 2012)


#baseline model prediction, look at the breakdown of the dependent varaible in the training set
table(train$Republican)

# 0  1 
# 47 53 

#in 47 of the 100 obeservations the democrat won the state, and in 53 the republican won
#baseline will always predict the more common outcome, always predicts 53% republican
#this is not a credible model, so we need to make a better baseline model.. like the Rasmussen variable

#the sign function will return -1 if passed a negative value, 1 if passed a positive value, 0 if 0
sign(test$Rasmussen)


table(sign(train$Rasmussen))

# -1  0  1 
# 41  3 56 

#in 56 cases the smart baseline prediced a republican win
#in 41 cases the smart baseline prediced a democrat


#training set outcome vs sign of polling data
table(train$Republican, sign(train$Rasmussen))

#     -1   0   1
#  0  41   2   4
#  1   0   1  52

#rows = true outcome
#columns = smart baseline predictions, -1, 0, 1
#52 obs where republican was predicted to win and did win
#41 obs where democrat was predicted to win and did win
#3 inconclusive
#4 mistakes
#smart baseline model is better than the naive baseline model

#build a logistic regression model


#is there multicolinearity within the models?

#this will not work because the State variable is non-numeric
cor(train) 

#take the correlation amongst the independent variables + the dependent variable
cor(train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])


#PropR is most highly correlated to Republican so we pick that for our log. regression model
mod1 = glm(Republican ~ PropR, data = train,family = binomial())

summary(mod1)

# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.22880  -0.06541   0.10260   0.10260   1.37392  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -6.146      1.977  -3.108 0.001882 ** 
#   PropR         11.390      3.153   3.613 0.000303 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 138.269  on 99  degrees of freedom
# Residual deviance:  15.772  on 98  degrees of freedom
# AIC: 19.772
# 
# Number of Fisher Scoring iterations: 8



#This model looks nice in terms of significance and the sign of coefficients
#PropR has a high coefficient that a republican will win
#AIC is ~19.8
#How will it do in predicting the republican outcome in the training set?

pred1 = predict(mod1, type = "response")
table(train$Republican,pred1 >= 0.5)

#   FALSE TRUE
# 0    45    2
# 1     2   51

#rows are the outcome (1 = republican, 0 = democrat)
#column true = predicted republican, column false = predicted democrat

#this model makes 4 mistakes on the training set, just about the same as the smart baseline model


cor(train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

#If 2 independent variables are very highly correlated, they are less likely to improve predictions
#Try to use 2 variables that are less correlated, like DiffCount+Rasmussen or DiffCount+SurveyUSA
mod2 = glm(Republican ~ SurveyUSA+DiffCount, data = train,family = binomial())
pred2 = predict(mod2, type = "response")
table(train$Republican,pred2 >= 0.5)
#made 1 less mistake in the training set

summary(mod2)
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.01196  -0.00698   0.01005   0.05074   1.54975  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  -1.1405     1.2456  -0.916   0.3599  
# SurveyUSA     0.2976     0.1949   1.527   0.1267  
# DiffCount     0.7673     0.4188   1.832   0.0669 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 138.269  on 99  degrees of freedom
# Residual deviance:  12.439  on 97  degrees of freedom
# AIC: 18.439
# 
# Number of Fisher Scoring iterations: 10


#AIC has a smaller value which suggests a stronger model
#Neither of the variables have a significance of a star or more, less significant statistically
#model2 is ok but not great


#now we evaluate our models on the test set.. smart baseline vs model1 vs model2

table(test$Republican, sign(test$Rasmussen))

#   -1  0  1
# 0 18  2  4
# 1  0  0 21

#Smart baseline predicted democrat would win 18 times and was correct
#Smart baseline predicted republican would win 21 times and was correct
#2 times inconclusive
#4 times predicted republican but democrat actually won


TestPrediction = predict(mod2,newdata = test,type="response")

table(test$Republican,TestPrediction > 0.5)

# FALSE TRUE
# 0    23    1
# 1     0   21

subset(test,TestPrediction >= 0.5 & Republican == 0)

#    State   Year Rasmussen SurveyUSA  DiffCount     PropR Republican
# 24 Florida 2012         2         0         6 0.6666667          0

# Model made a mistake with Florida but overall it does well

