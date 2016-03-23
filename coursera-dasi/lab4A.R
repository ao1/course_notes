load(url("http://bit.ly/dasi_nc"))

#Exploratory analysis

str(nc)
summary(nc)

gained_clean = na.omit(nc$gained)
n = length(gained_clean)

length(nc$gained)
length(gained_clean)

#The bootstrap
#construct a bootstrap confidence interval

boot_means = rep(NA, 100)

for(i in 1:100){
  boot_sample = sample(gained_clean, n, replace = TRUE)
  boot_means[i] = mean(boot_sample)                       
}

hist(boot_means)

#90% interval estimation using percentile method
n = 100 #100 samples
c = 0.90 # confidence

middle = n*c
tails = n-middle

dotchart(boot_means)
#looking at plot, estimate..  (29.6,30.7)

#90% interval estimation using std err method
sample_median = median(boot_means)
df = n-1
se_boot = sd(boot_means)
t_star = tstar = abs(qt(0.05,df=df)) 

sample_median + t_star*se_boot
sample_median - t_star*se_boot
#(29.6,30.9)

source("http://bit.ly/dasi_inference")

#By default the function takes 10,000 bootstrap samples 
#(instead of the 100 youve taken above), creates a bootstrap 
#distribution, and calculates the confidence interval, using 
#the percentile method.

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.90, est = "mean", boot_method = "perc")

#adjust cl to 95%
inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "perc")
#use std error estimation method instead of percentile 
inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "se")
#create interval for median instead of mean
inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "median", boot_method = "se")

inference(nc$fage, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "se")

temp1 = subset()



















