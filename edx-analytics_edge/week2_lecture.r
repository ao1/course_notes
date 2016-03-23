#Set locale to USA
Sys.setlocale("LC_ALL", "C")

default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")

wine = read.csv(file.path(default_path,"wine.csv"))
#wine_test = read.csv(file.path(default_path,"wine_test.csv"))
str(wine)
# 'data.frame':	25 obs. of  7 variables:
# $ Year       : int  1952 1953 1955 1957 1958 1959 1960 1961 1962 1963 ...
# $ Price      : num  7.5 8.04 7.69 6.98 6.78 ...
# $ WinterRain : int  600 690 502 420 582 485 763 830 697 608 ...
# $ AGST       : num  17.1 16.7 17.1 16.1 16.4 ...
# $ HarvestRain: int  160 80 130 110 187 187 290 38 52 155 ...
# $ Age        : int  31 30 28 26 25 24 23 22 21 20 ...
# $ FrancePop  : num  43184 43495 44218 45152 45654 ...


#create a 1 variable linear regression equation using only AGST to predict price
model1 = lm(Price ~ AGST, data=wine)

summary(model1)
# Call:
#   lm(formula = Price ~ AGST, data = wine)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.78450 -0.23882 -0.03727  0.38992  0.90318 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -3.4178     2.4935  -1.371 0.183710    
# AGST          0.6351     0.1509   4.208 0.000335 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4993 on 23 degrees of freedom
# Multiple R-squared:  0.435,	Adjusted R-squared:  0.4105 
# F-statistic: 17.71 on 1 and 23 DF,  p-value: 0.000335


#Residuals = error terms
#Intercept term = the expected mean value of Y when all X=0
#AGST = independent varaible
#Estimate = estimate of beta values for our model
#Multiple R-squared = will increase with new variables
#Adjusted R-squared = will decrease if you add independent variables that don't help the model
#Adjusted R-squared = adjusts R squared to account for the number of independent variables used, relative to the number of data points. 


#add the HarvestRain varaible to the regression model
model2 = lm(Price ~ AGST + HarvestRain,data=wine)

#compute SSE (Sum of Squared Errors) for model1 and model2
sum(model1$residuals^2)
sum(model2$residuals^2)

#5.734875 vs 2.970373, model2 SSE is much better

#create a linear regression model to predict Price using HarvestRain and WinterRain as independent variables
testmodel1 = lm(Price ~ HarvestRain + WinterRain,data=wine)
testmodel1$coefficients

#make a new linear regression model with a lot
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop,data=wine)

sum(model3$residuals^2)
#model3 SSE is even better than model2, but should we keep all these variables?

summary(model3)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.48179 -0.24662 -0.00726  0.22012  0.51987 
# 
# Coefficients:
#
#              Estimate   Std. Error  t value Pr(>|t|)    
# (Intercept) -4.504e-01  1.019e+01  -0.044 0.965202    
# AGST         6.012e-01  1.030e-01   5.836 1.27e-05 ***
# HarvestRain -3.958e-03  8.751e-04  -4.523 0.000233 ***
# WinterRain   1.043e-03  5.310e-04   1.963 0.064416 .  
# Age          5.847e-04  7.900e-02   0.007 0.994172    
# FrancePop   -4.953e-05  1.667e-04  -0.297 0.769578    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3019 on 19 degrees of freedom
# Multiple R-squared:  0.8294,	Adjusted R-squared:  0.7845 
# F-statistic: 18.47 on 5 and 19 DF,  p-value: 1.044e-06

# Independent variables listed on the left
# Estimate - coefficient of 0 means the value does not change prediction, remove it from model
# Std Error - how much the coefficient is likely to vary from the estimate value
# T Value - estimate / std error, larger the abs(t-value) the more likely it is significant
# Pr(>|t|) - How plausible it is that coefficient is 0, we want small values here
# *** - 3 stars = highest level of significance, 2 stars = very significant, 1 star = somewhat significant
#       . = almost significant, no stars or dots means the variable is not significant


#based on the above, we should remove FrancePop because it is insignificant
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age,data=wine)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.45470 -0.24273  0.00752  0.19773  0.53637 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3.4299802  1.7658975  -1.942 0.066311 .  
# AGST         0.6072093  0.0987022   6.152  5.2e-06 ***
# HarvestRain -0.0039715  0.0008538  -4.652 0.000154 ***
# WinterRain   0.0010755  0.0005073   2.120 0.046694 *  
# Age          0.0239308  0.0080969   2.956 0.007819 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.295 on 20 degrees of freedom
# Multiple R-squared:  0.8286,	Adjusted R-squared:  0.7943 
# F-statistic: 24.17 on 4 and 20 DF,  p-value: 2.036e-07

#Note: Age was not significant when we had FrancePop. Now it is. 

#Age and FrancePop are highly correlated.
cor(wine$Age,wine$FrancePop) #-0.9944851 

#Corelation of 2 varaibles: 1 = perfectly positive, -1 = perfectly negative, 0 = no linear relationship


cor(wine$WinterRain,wine$Price) #0.1366505, not much correlation
cor(wine$Age,wine$Price) #0.4477679

#show all correlation combinations in a table
cor(wine)

#Multi-colinearity = 2 independent varaibles related
#A high correlation between an independent and a dependent variable is good


#try taking out Age
model5 = lm(Price ~ AGST + HarvestRain + WinterRain,data=wine)
#r squared got lower/worse, so we keep Age (use model4)
#try removing 1 variable at a time, apply common sense to which variable to keep
#coliniarity = coefficients are only interpretable in the presence of other variables being used

#typically a correlation of 0.7+ or 0.7- is bad

#use choose model4 - Multiple R-squared:  0.8286,	Adjusted R-squared:  0.7943 
#this is an indicator of how well the model does on the current data, but what about new data?

#training data = data that we used to create the model
#test data = data that we will use to test the model
#out of sample accuracy  = accuracy of the model on test data

#write in test data which has only 2 data points
winetest = read.csv(file.path(default_path,"wine_test.csv"))

#apply model4 to the test data
predict_test = predict(model4,newdata = winetest)

# 1        2 
# 6.768925 6.684910 

# 
# The formula for R2 is
# 
# R2 = 1 - SSE/SST,
# 
# where SST is calculated using the average value of the dependent variable on the training set.
# 
# Since SSE and SST are the sums of squared terms, we know that both will be positive. 
# Thus SSE/SST must be greater than or equal to zero. This means it is not possible to have 
# an out-of-sample R2 value of 2.4.
# 
# However, all other values are valid (even the negative ones!), since SSE can be more or less 
# than SST, due to the fact that this is an out-of-sample R2, not a model R2. 

SSE = sum((winetest$Price - predict_test)^2)
SST = sum((winetest$Price - mean(wine$Price))^2)

1-(SSE/SST) #0.7944278 is a good r-squrared, but the test set is very small (only 2 data points)

#training data r-squared will always be higher with more varaibles
#test data r-squated will vary
#a negative r-squared means the test set data is out of sample, which is bad
#we want to pick the model which has the highest r-squared for both the training and test data


baseball = read.csv(file.path(default_path,"baseball.csv"))
str(baseball)

#make a subset with only teams up until 2002
moneyball = subset(baseball, Year < 2002)

#add new variable: run difference = runs scored - runs allowed
moneyball$RD = moneyball$RS - moneyball$RA

#plot Run difference vs Wins, plot shows that there is a strong linear relationship
plot(moneyball$RD,moneyball$W)

WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -14.2662  -2.6509   0.1234   2.9364  11.6570 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 80.881375   0.131157  616.67   <2e-16 ***
#   RD           0.105766   0.001297   81.55   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.939 on 900 degrees of freedom
# Multiple R-squared:  0.8808,	Adjusted R-squared:  0.8807 
# F-statistic:  6651 on 1 and 900 DF,  p-value: < 2.2e-16

#We see RD is very significant (3 stars) and the r-squared is 0.88, 
#we have a strong model to predict wins using runs scored - runs allowed
#
#W = 80.881375 + 0.105766 * (RD)
#W >= 95
#W = 80.881375 + 0.105766 * (RD)
#
#RD >= ( 95 - 80.881375 ) / 0.1058 = 133.4 ~ 135
#
#A team needs to score 135 more runs than they allow, to win 95 games
#
#If a baseball team scores 713 runs and allows 614 runs, 
#how many games do we expect the team to win?
#
#Wins = InterceptCoeficient + RunDifferenceCoefficient * (RunDifference) 
#Wins = 80.881375 + 0.105766 * (713-614)
#Wins = 91.352209
#
#
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
#we see that the coefficient for BA is negative, multi-colinearity, remove BA

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
#r-squared similar, all coefficients positive, overall a better model
#OBP 2737.77, SLG 1584.91, means On Base % is worth more


# Models:
# Intercept Coefficient + var1_coefficient*real_value1 + var2_coefficient*real_value2
# Wins =  80.8814 + 0.1058*(RS - RA)
# Runs Scored = -804.63 + 2737.77*(OOBP) + 1584.91*(OSLG)
# Runs Allowed = -837.38 + 2913.60*(OOBP) + 1514.29*(OSLG)

#playoffs suffer from an insufficient sample size problem, not enough games to judge

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)







