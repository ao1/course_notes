load(url("http://www.openintro.org/stat/data/kobe.RData"))

head(kobe)
summary(kobe)

#For this lab, we define the length of a shooting streak to be the number of consecutive baskets made until a miss occurs.

kobe$basket[1:9]

kobe_streak = calc_streak(kobe$basket)
barplot(table(kobe_streak))
median(sort(kobe_streak))
boxplot(kobe_streak)

#draw one element from input list c("heads","tails")
sample(c("heads","tails"), size = 1, replace = TRUE)

#draw one element 100 times from input list c("heads","tails")
sim_fair_coin = sample(c("heads","tails"), size = 100, replace = TRUE)
prop.table(table(sim_fair_coin))

#add a probability list, to simulate an unfair coin
sim_unfair_coin = sample(c("heads","tails"), size = 100, replace = TRUE,prob = c(0.2,0.8))
prop.table(table(sim_unfair_coin))

# prob = c(0.2,0.8) indicates that for the two elements in the outcomes vector, 
# we want to select the first one, heads, with probability 0.2 and the second one, 
# tails with probability 0.8. Another way of thinking about this is to 
# think of the outcome space as a bag of 10 chips, where 2 chips are 
# labeled "head" and 8 chips "tail". Therefore at each draw, the 
# probability of drawing a chip that says "head" is 20%, and "tail" is 80%.

# To simulate a single shot from an independent shooter with a shooting percentage of 50% we type:
outcomes = c("H", "M")
sim_basket = sample(outcomes, size = 1, replace = TRUE)

# Simulate 133 shots with a shot probability of 0.45
sim_basket = sample(outcomes, size = 133, replace = TRUE,prob=c(0.45,0.55))

# Both data sets represent the results of 133 shot attempts, each 
# with the same shooting percentage of 45%. We know that our simulated 
# data is from a shooter that has independent shots. That is, we know 
# the simulated shooter does not have a hot hand.

prop.table(table(sim_basket))
prop.table(table(kobe$basket,sim_basket))

sim_streak = calc_streak(sim_basket)

barplot(table(sim_streak))
barplot(table(kobe_streak))
boxplot(sim_streak)

#========================================================

pnorm(110,mean=110,sd=15)
qnorm(0.90,mean=1500,sd=300)
1-pnorm(50,mean=45,sd=3.2)
qnorm(0.20,mean=77,sd=5)

#prob exactlty 35 out of 3000000 will happen at p
dbinom(35,size=3000000,p=0.00001)

#prob more than 35 out of 3000000 will happen at p
1-pbinom(35,size=3000000,p=0.00001)

#normal approximation
sum(dbinom(35:40,size=3000000,p=0.00001))


#probability exactlty 2 out of 3 kids will be boys, p of having a boy = 0.51
dbinom(2,size=3,p=0.51)

dbinom(10,size=100,p=0.24)





