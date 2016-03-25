#Set locale to USA
Sys.setlocale("LC_ALL", "C")

default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")


#The Logit is just log(Odds), and looks like the linear regression equation. 

#Logit = -1.5 + 3*1 - 0.5*5 = -1.
#Odds = e^(-1) = 0.3678794.
#P(y = 1) = 1/(1 + e^(-Logit)) = 1/(1 + e^(1)) = 0.2689414.

quality = read.csv(file.path(default_path,"quality.csv"))

str(quality)

# MemberID numbers the patients from 1 to 131, and is just an identifying number.
# InpatientDays is the number of inpatient visits, or number of days the person spent in the hospital.
# ERVisits is the number of times the patient visited the emergency room.
# OfficeVisits is the number of times the patient visited any doctor's office.
# Narcotics is the number of prescriptions the patient had for narcotics.
# DaysSinceLastERVisit is the number of days between the patient's last emergency room visit and the end of the study period (set to the length of the study period if they never visited the ER). 
# Pain is the number of visits for which the patient complained about pain.
# TotalVisits is the total number of times the patient visited any healthcare provider.
# ProviderCount is the number of providers that served the patient.
# MedicalClaims is the number of days on which the patient had a medical claim.
# ClaimLines is the total number of medical claims.
# StartedOnCombination is whether or not the patient was started on a combination of drugs to treat their diabetes (TRUE or FALSE).
# AcuteDrugGapSmall is the fraction of acute drugs that were refilled quickly after the prescription ran out.
# PoorCare is the outcome or dependent variable, and is equal to 1 if the patient had poor care, and equal to 0 if the patient had good care.

#dependent variable = PoorCare
#independent variable = everything else, except for MemberID

table(quality$PoorCare) #98 received good care, 33 received poor care

#baseline model will have an accuracy of 98/131=0.7480916
#we are trying to beat the baseline model with logistic regression

#install caTools
#install.packages("caTools") #add caTools package

#import caTools
library(caTools)


#initialize random number generator
set.seed(88) 
#split quality.csv into a training set and testing set using sample.split
#1st argument = outcome variable
#2nd argument = the percentage of data we want for training set
split = sample.split(quality$PoorCare,SplitRatio = 0.75)

split #TRUE = for training set , FALSE = for testing set

qualityTrain = subset(quality, split == TRUE) #99 observations
qualityTest = subset(quality, split == FALSE) #32 observations

#make the logistic regression model using OfficeVisits and Narcotics as independent variables
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)

#Coefficients table gives betas, higher values = indicative of poor care
#The coefficient value is positive, meaning that positive values of the 
#variable make the outcome of 1 more likely.

#1+ star = significant
#AIC value = measure of quality of the model, like adjusted r squared
#AIC can only be compared between models using the same data set
#AIC = lower is better
summary(QualityLog)

#response type gives probablities
predictTrain = predict(QualityLog, type="response")

#all of the numbers should be between 0 and 1
summary(predictTrain)

#avg (mean) prediction for each of the outcomes
tapply(predictTrain,qualityTrain$PoorCare,mean)
#for false: 0.1894512 
#for true: 0.4392246 
#good sign, predicting higher for true cases


QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)

summary(QualityLog2)

#will return true if prediction > 0.5 
table(qualityTrain$PoorCare,predictTrain > 0.5)

#   FALSE TRUE
# 0    70    4
# 1    15   10

#For 70 cases we predict good care, and they actually receive good care
#For 10 cases we predict poor care, and they actually receive poor care
#We make 4 mistakes where we predict poor care, and they actually receive good care
#We make 15 mistakes where we predict good care, and they actually receive poor care

#sensitivity = 0.4
10/25

#specificity = 0.9459459
70/74


#increase the threshold to 0.7
table(qualityTrain$PoorCare,predictTrain > 0.7)

# FALSE TRUE
# 0    73    1
# 1    17    8

#sensitivity = 0.32
8/25

#specificity = 0.9864865
73/74

#by increasing the threshold our sensitivity went down and specificity went up

#decrease the threshold to 0.2
table(qualityTrain$PoorCare,predictTrain > 0.2)

# FALSE TRUE
# 0    54   20
# 1     9   16

#sensitivity = 0.64  went up
#specificity = 0.7297297  went down

#which threshold should we pick?

#practice problem:

#specificity of Confusion Matrix #1 = 0.6
#sensitivity of Confusion Matrix #1 = 0.8

#specificity of Confusion Matrix #2 = 0.8
#sensitivity of Confusion Matrix #2 = 0.6

#increasing the threshold -> sensitivity goes down and specificity goes up

#we can pick a threshold by using a ROC Curve, using the ROCR package
#install.packages("ROCR")
library(ROCR)

ROCRpred = prediction(predictTrain,qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred,"tpr","fpr")

#plot
plot(ROCRperf)

#add colors
plot(ROCRperf,colorize=TRUE)

#add labels
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))



#Compute the test set predictions by running the command:
predictTest = predict(QualityLog, type="response", newdata=qualityTest)

#You can compute the test set AUC by running the following two commands:
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


# The AUC of a model has the following nice interpretation: given a random patient 
# The AUC of a model has the following nice interpretation: given a random patient 
# from the dataset who actually received poor care, and a random patient from the 
# dataset who actually received good care, the AUC is the perecentage of time that 
# our model will classify which is which correctly.







