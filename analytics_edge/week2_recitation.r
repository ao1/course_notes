#Set locale to USA
Sys.setlocale("LC_ALL", "C")

default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")

NBA = read.csv(file.path(default_path,"NBA_train.csv"))


str(NBA)

#if a variable begins with a number, R will put an X in front of it

#how many games does a team need to win to get into the playoffs?
table(NBA$W,NBA$Playoffs)
plot(NBA$W,NBA$Playoffs)
#42

NBA$PTSdiff = NBA$PTS - NBA$oppPTS

plot(NBA$W,NBA$PTSdiff)
#there is a linear relationship bewteen wins and (points scored-opponent points)

WinsReg = lm(W ~ PTSdiff,data=NBA)

# summary(WinsReg)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.7393 -2.1018 -0.0672  2.0265 10.6026 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.100e+01  1.059e-01   387.0   <2e-16 ***
#   PTSdiff     3.259e-02  2.793e-04   116.7   <2e-16 ***

#very significant variables and a high r-squared, strong linear relationship

#PTSdiff = PTS - oppPTS

#Wins = 41 + 0.03259 * (PTSdiff)

#if you want to check for Wins >= 42

#PTSdiff >= 42-41 / 0.03259 = 30.67 ~ 31
#need to score at least 31 more points than we allow to win at least 42 games


PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=NBA)
 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.051e+03  2.035e+02 -10.078   <2e-16 ***
#   X2PA         1.043e+00  2.957e-02  35.274   <2e-16 ***
#   X3PA         1.259e+00  3.843e-02  32.747   <2e-16 ***
#   FTA          1.128e+00  3.373e-02  33.440   <2e-16 ***
#   AST          8.858e-01  4.396e-02  20.150   <2e-16 ***
#   ORB         -9.554e-01  7.792e-02 -12.261   <2e-16 ***
#   DRB          3.883e-02  6.157e-02   0.631   0.5285    
# TOV         -2.475e-02  6.118e-02  -0.405   0.6859    
# STL         -1.992e-01  9.181e-02  -2.169   0.0303 *  
#   BLK         -5.576e-02  8.782e-02  -0.635   0.5256    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 185.5 on 825 degrees of freedom
# Multiple R-squared:  0.8992,	Adjusted R-squared:  0.8981 
# F-statistic: 817.3 on 9 and 825 DF,  p-value: < 2.2e-16

PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
#lots of errors

#root means squared error
RMSE = sqrt(SSE/nrow(NBA))
#on avg we make an error every 184.4049 points

#avg number of points in a season is 8370.24
mean(NBA$PTS)

#being off by 184 points is not bad, but we can improve by removing some varaibles
#remove the variable with the highest |t| value.. TOV, higher means less significant


PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=NBA)
PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=NBA)
PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -523.33 -122.02    6.93  120.68  568.26 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.033e+03  1.629e+02 -12.475  < 2e-16 ***
#   X2PA         1.050e+00  2.829e-02  37.117  < 2e-16 ***
#   X3PA         1.273e+00  3.441e-02  37.001  < 2e-16 ***
#   FTA          1.127e+00  3.260e-02  34.581  < 2e-16 ***
#   AST          8.884e-01  4.292e-02  20.701  < 2e-16 ***
#   ORB         -9.743e-01  7.465e-02 -13.051  < 2e-16 ***
#   STL         -2.268e-01  8.350e-02  -2.717  0.00673 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 185.3 on 828 degrees of freedom
# Multiple R-squared:  0.8991,	Adjusted R-squared:  0.8983 
# F-statistic:  1229 on 6 and 828 DF,  p-value: < 2.2e-16

#double check SSE and RMSE now
SSE4 = sum(PointsReg4$residuals^2)
#28421465, hard to interpret

RMSE4 = sqrt(SSE4 / nrow(NBA))
#no change, have simpler model with the same amount of error

#use model4 from above on test data
NBA_test = read.csv(file.path(default_path,"NBA_test.csv"))

PointsPredictions = predict(PointsReg4, newdata = NBA_test)

#Out of sample r-squared, test of how model performs on other data

SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE / SST  #0.8127142
RMSE = sqrt(SSE / nrow(NBA_test))  #196.3723 higher but not bad









