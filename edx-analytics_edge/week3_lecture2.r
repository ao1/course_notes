Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
} else {
  next
}

framingham = read.csv(file.path(default_path,"framingham.csv"))

# to predict whether or not a patient experienced CHD within 10 years of first examination

# 1. split patients into training set and testing set
# 2. use logistic regression to predict
# 3. evaluate the predictive power of the model on the test set

str(framingham)

#4240 obs. /  16 variables
#TenYearCHD is the dependent variable

#load caTools
library(caTools)

#set random seed and create split, put 65% of the data in training set
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

#if you have a lot of data, you can afford to put less in the training set and more in testing set
#you typically want 50-80% of the data in the training set

#create the train and test sets based on split variable boolean
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

#create logistic regression model, the period means use all other variables as independent variables
#never use the period if you have ID variables like name or ID number
framinghamLog = glm(TenYearCHD ~ .,data = train, family = binomial)

summary(framinghamLog)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.8487  -0.6007  -0.4257  -0.2842   2.8369  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -7.886574   0.890729  -8.854  < 2e-16 ***
#   male             0.528457   0.135443   3.902 9.55e-05 ***
#   age              0.062055   0.008343   7.438 1.02e-13 ***
#   education       -0.058923   0.062430  -0.944  0.34525    
# currentSmoker    0.093240   0.194008   0.481  0.63080    
# cigsPerDay       0.015008   0.007826   1.918  0.05514 .  
# BPMeds           0.311221   0.287408   1.083  0.27887    
# prevalentStroke  1.165794   0.571215   2.041  0.04126 *  
#   prevalentHyp     0.315818   0.171765   1.839  0.06596 .  
# diabetes        -0.421494   0.407990  -1.033  0.30156    
# totChol          0.003835   0.001377   2.786  0.00533 ** 
#   sysBP            0.011344   0.004566   2.485  0.01297 *  
#   diaBP           -0.004740   0.008001  -0.592  0.55353    
# BMI              0.010723   0.016157   0.664  0.50689    
# heartRate       -0.008099   0.005313  -1.524  0.12739    
# glucose          0.008935   0.002836   3.150  0.00163 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2020.7  on 2384  degrees of freedom
# Residual deviance: 1792.3  on 2369  degrees of freedom
# (371 observations deleted due to missingness)
# AIC: 1824.3
# 
# Number of Fisher Scoring iterations: 5


#Note: all significant variables have a positive coefficient, 
#      higher values in this variables contribute to a higher probability in 10 year CHD


#use this model to make predictions on the test set
predictTest = predict(framinghamLog,type="response",newdata = test)


#create a confusion matrix with a threshold of 0.5
table(test$TenYearCHD,predictTest > 0.5)
 
# FALSE TRUE
# 0  1069    6
# 1   187   11

#accuracy of the model: 0.8483896 = 84.8%
(1069+11)/(1069+6+187+11)

#accuracy of a baseline method = 84.4%
(1069+6)/(1069+6+187+11)

#our model barely beats the baseline in terms of accuracy

#do we still have a viable model by varying the threshold?
#to find out, we compute the out of sample AUC

#import ROCR package
library(ROCR)

#prediction function of the ROCR package
ROCRpred = prediction(predictTest,test$TenYearCHD)

#0.7421095 = 74% AUC
as.numeric(performance(ROCRpred,"auc")@y.values)

# Properties of this model:
# - accuracy was close to the baseline model
# - rarely predicted a 10 year CHD risk above 50%
# - model can differentiate between low risk and high risk patients well (AUC=74%)
# - Smoking/Cholesterol/Systolic Blood Pressure/Glucose are suggested as significant variables
# - Unclear if this model generalizes to other populations since Farmingham people are white/middle class

#External validation: extending a model to other types of populations

#Be as conservative as possible about doing external validation



















