#The details of every real estate transaction in Ames is recorded 
#by the City Assessor's office. Our particular focus for this lab 
#will be all residential home sales in Ames between 2006 and 2010.

load(url("http://www.openintro.org/stat/data/ames.RData"))


area = ames$Gr.Liv.Area
price = ames$SalePrice

#lookup if right skewed and unimodal
hist(area,breaks = 50)
#lookup mean, min, max and 1st and 3rd quartile
summary(area)
#lookup min, max
min(area)
max(area)
#lookup std dev
sd(area)

quantile(area)

#lookup iqr
IQR(area)
boxplot(area)

#mean = 150
mean(area)

#a simple random sample of size 50
samp0 = sample(area, 50)
samp1 = sample(area, 50)
samp2 = sample(area, 100)
samp3 = sample(area, 1000)

mean(samp2)

#Generate 5000 samples and compute the sample mean of each
sample_means50 = rep(NA, 5000)

for(i in 1:5000){
  samp = sample(area, 50)
  sample_means50[i] = mean(samp)
}

hist(sample_means50, breaks = 25)


#Generate 100 samples and compute the sample mean of each
sample_means_small = rep(NA, 100)

for(i in 1:100){
  samp = sample(area, 50)
  sample_means_small[i] = mean(samp)
}

length(sample_means50)
length(sample_means_small)

# To get a sense of the effect that sample size has on our sampling 
# distribution, let's build up two more sampling distributions: 
# one based on a sample size of 10 and another based on a 
# sample size of 100.

sample_means10 = rep(NA, 5000)
sample_means100 = rep(NA, 5000)

for(i in 1:5000){
  samp = sample(area, 10)
  sample_means10[i] = mean(samp)
  samp = sample(area, 100)
  sample_means100[i] = mean(samp)
}


# To see the effect that different sample sizes have on the 
# sampling distribution, plot the three distributions on 
# top of one another

# The xlim argument specifies the range of the x-axis of the 
# histogram, and by setting it equal to xlimits for each 
# histogram, we ensure that all three histograms will be 
# plotted with the same limits on the x-axis.

par(mfrow = c(3, 1))

xlimits = range(sample_means10)

hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)

#To return to the default setting of plotting one plot 
#at a time, run the following command:
par(mfrow = c(1, 1))

#Take a random sample of size 50 from price. Using this sample, 
#what is your best point estimate of the population mean?

sample_price_means50 = rep(NA, 50)

for(i in 1:50){
  samp = sample(price, 10)
  sample_price_means50[i] = mean(samp)
}

# Part 2

# Start with a simple random sample of size 60 from the population. 
# Note that the data set has information on many housing variables, 
# but for the first portion of the lab we'll focus on the size of 
# the house, represented by the variableGr.Liv.Area.

population = ames$Gr.Liv.Area

mean(population)

samp = sample(population, 60)

hist(samp,breaks = 25)

mean(samp)
mean(population)

sample_mean = mean(samp)

# That serves as a good point estimate but it would be useful 
# to also communicate how uncertain we are of that estimate. 
# This can be captured by using a confidence interval.

# We can calculate a 95% confidence interval for a sample mean 
# by adding and subtracting 1.96 standard errors to the point 
# estimate. 

se = sd(samp)/sqrt(60)
lower = sample_mean - 1.96 * se
upper = sample_mean + 1.96 * se
c(lower, upper)

# This is an important inference that we've just made: 
# even though we don't know what the full population 
# looks like, we're 95% confident that the true average 
# size of houses in Ames lies between the values lower 
# and upper. 

# .	Step 1: Obtain a random sample.
# .	Step 2: Calculate the sample's mean and standard deviation.
# .	Step 3: Use these statistics to calculate a confidence interval.
# .	Step 4: Repeat steps (1)-(3) 50 times.

samp_mean = rep(NA, 50)
samp_sd = rep(NA, 50)
n = 60

for(i in 1:50){
  samp = sample(population, n) # obtain a sample of size n = 60 from the population
  samp_mean[i] = mean(samp)    # save sample mean in ith element of samp_mean
  samp_sd[i] = sd(samp)        # save sample sd in ith element of samp_sd
}

lower = samp_mean - 1.96 * samp_sd / sqrt(n) 
upper = samp_mean + 1.96 * samp_sd / sqrt(n)

c(lower[1],upper[1])
plot_ci(lower, upper, mean(population))













