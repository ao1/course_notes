source("http://bit.ly/dasi_inference")
load(url("http://www.openintro.org/stat/data/atheism.RData"))

us12 = subset(atheism, atheism$nationality == "United States" & atheism$year == "2012")

prop.table(table(us12$response))

#Note that since the goal is to construct an interval estimate 
#for a proportion, its necessary to specify what constitutes a 
#success, which here is a response of atheist.

n = 1000
p = seq(0, 1, 0.01)
me = 2*sqrt(p*(1 - p)/n)
plot(me ~ p)


spain = subset(atheism, atheism$nationality == "Spain")
spain05 = subset(atheism, atheism$nationality == "Spain" & atheism$year == "2005")
spain12 = subset(atheism, atheism$nationality == "Spain" & atheism$year == "2012")

prop.table(table(spain$response,spain$year))

inference(spain05$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")
inference(spain12$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")

us = subset(atheism, atheism$nationality == "United States")
us05 = subset(atheism, atheism$nationality == "United States" & atheism$year == "2005")
us12 = subset(atheism, atheism$nationality == "United States" & atheism$year == "2012")

inference(us05$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")
inference(us12$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")

prop.table(table(us$response,us$year))
