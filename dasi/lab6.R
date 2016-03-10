load(url("http://www.openintro.org/stat/data/mlb11.RData"))

plot(mlb11$runs,mlb11$at_bats)
cor(mlb11$runs, mlb11$at_bats)
mlb11[mlb11$runs > 800,] # show the 3 outliers

# Use the following interactive function to select the line that 
# you think does the best job of going through the cloud of points.

plot_ss(x = mlb11$at_bats, y = mlb11$runs)

# Call:
#   lm(formula = y ~ x, data = pts)
# 
# Coefficients:
#   (Intercept)            x  
# -8982.810        1.756  
# 
# Sum of Squares:  370365.9

#The most common way to do linear regression is to select the 
#line that minimizes the sum of squared residuals. 
#To visualize the squared residuals, you can rerun the plot 
#command and add the argument showSquares = TRUE.

plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

# 480904.1
# 565457.9
# 504200

# It is rather cumbersome to try to get the correct least squares line, 
# i.e. the line that minimizes the sum of squared residuals, 
# through trial and error. Instead we can use the lm function 
# in R to fit the linear model (a.k.a. regression line).

m1 = lm(runs ~ at_bats, data = mlb11)

# The first argument in the function lm is a formula that takes the form y ~ x. 
# Here it can be read that we want to make a linear model of runs 
# as a function of at_bats. 
# The second argument specifies that R should look in the mlb11 data frame 
# to find the runs and at_bats variables.

summary(m1)

# The formula used to describe the model 

# Call:
#   lm(formula = runs ~ at_bats, data = mlb11)



# Five-number summary of the residuals

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -125.58  -47.05  -16.59   54.40  176.87 



# The linear model's y-intercept and the coefficient of at_bats

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2789.2429   853.6957  -3.267 0.002871 ** 
#   at_bats         0.6305     0.1545   4.080 0.000339 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



# Multiple R-squared, or more simply, R2. The R2 value represents 
# the proportion of variability in the response variable that is 
# explained by the explanatory variable. For this model, 37.3% of 
# the variability in runs is explained by at-bats.

# Residual standard error: 66.47 on 28 degrees of freedom
# Multiple R-squared:  0.3729,	Adjusted R-squared:  0.3505 
# F-statistic: 16.65 on 1 and 28 DF,  p-value: 0.0003388

m2 = lm(runs ~ homeruns, data = mlb11)
summary(m2)

# Create a scatterplot with the least squares line laid on top.

plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

mlb11$runs[mlb11$at_bats == 5579]










