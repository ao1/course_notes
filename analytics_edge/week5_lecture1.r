#Set locale to USA
Sys.setlocale("LC_ALL", "C")


if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
  
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
  
} else {
  
  print('Could not set default path')
  
}


#add stringsAsFactors because we are dealing with text data
tweets = read.csv(file.path(default_path,"tweets.csv"),stringsAsFactors = FALSE) 

#
# Pre-Process data
#


#add extra variable for negative tweets
tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)
prop.table(table(tweets$Negative)) #0.1541067 prop of negative tweets

#install.packages("tm")
#install.packages("SnowballC")

library("tm")
library("SnowballC")

#create corpus from tweet column of our data frame
corpus = Corpus(VectorSource(tweets$Tweet))

#check if it worked
corpus
corpus[[1]]$content

### switch all text to lower case ###
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

#check if switch worked
corpus[[1]]$content

### remove all punctucation ###
corpus = tm_map(corpus, removePunctuation)

#check if it worked
corpus[[1]]$content

#show first 10 english stopwords
stopwords("english")[1:10]

#make a vector with all english stopwords + "apple"
applestopwords = c("apple", stopwords("english"))

### remove stopwords ###
corpus = tm_map(corpus, removeWords, applestopwords )

#check if it worked
corpus[[1]]$content

### stem document ###
corpus = tm_map(corpus, stemDocument)

#check if it worked
corpus[[1]]$content

#
# Extract word frequencies from data
#

frequencies = DocumentTermMatrix(corpus)

# <<DocumentTermMatrix (documents: 1181, terms: 3289)>>
#   Non-/sparse entries: 8980/3875329
# Sparsity           : 100%
# Maximal term length: 115
# Weighting          : term frequency (tf)

# 1181 tweets after pre-processing, 3289 words

#frequencies[start doc # : end doc # , start word index : end word index]
inspect(frequencies[1000:1005,505:515])

#sparse data (many 0's in matrix)

#adjust frequency threshold, show words that appear 20+ times
findFreqTerms(frequencies , lowfreq = 20)

#more terms = more ind variables = more runtime

#remove terms that don't appear often,
sparse = removeSparseTerms(frequencies , 0.995)
sparse

# <<DocumentTermMatrix (documents: 1181, terms: 309)>>
#   Non-/sparse entries: 4669/360260
# Sparsity           : 99%
# Maximal term length: 20
# Weighting          : term frequency (tf)

#309 words, 9% of previous amount of words


#convert sparse matrix into data frame
tweetsSparse = as.data.frame(as.matrix(sparse))

#convert variable names to make sure they are appropriate names
#do this once for each text data frame
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

tweetsSparse$Negative = tweets$Negative

#
# Create train and test set
#

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative , SplitRatio = 0.7)

train = subset(tweetsSparse, split == TRUE)
test = subset(tweetsSparse, split == FALSE) 


#
# Use CART to create a predictive model
#

library(rpart)
library(rpart.plot)

CARTtweet = rpart(Negative ~ . , data = train , method="class")

prp(CARTtweet)

predict_cart = predict(CARTtweet , newdata = test , type="class")

#make confusion matrix
table(test$Negative , predict_cart)

sum(diag(table(test$Negative , predict_cart))) / sum(table(test$Negative , predict_cart))
#accuracy = 0.8788732 

#compare to baseline model of always non-negative
prop.table(table(test$Negative))
#0.8450704


#
# Use Random Forest to create a predictive model
#

library(randomForest)

set.seed(123)

#will take a long time because we have 300 variables, RF takes longer than CART
RFtweet = randomForest(Negative ~ . , data = train)

predict_rf = predict(RFtweet , newdata = test , type="class")

#make confusion matrix
table( test$Negative , predict_rf)

sum(diag(table(test$Negative , predict_rf))) / sum(table(test$Negative , predict_rf))
#accuracy = 0.884507

#accuracy of Random Forest is higher than CART, but CART is more interpretable
#using cp on CART would increase its accuracy


#
# Building a logistic regression model
#


lr1 = glm(Negative ~ . , data = train , family="binomial")


predictions = predict(lr1, newdata=test, type="response")
predictions_train = predict(lr1, newdata=train , type="response")

#Build a confusion matrix (with a threshold of 0.5) 

table( test$Negative , predictions > 0.5 )

sum(diag( table( test$Negative , predictions > 0.5 ) ))  / sum(table( test$Negative , predictions > 0.5 ))
#accuracy 0.8056338

# The accuracy is (254+37)/(254+46+18+37) = 0.8197183, 
# which is worse than the baseline. If you were to compute the 
# accuracy on the training set instead, you would see that the 
# model does really well on the training set - this is an example 
# of over-fitting. The model fits the training set really well, 
# but does not perform well on the test set. A logistic regression 
# model with a large number of variables is particularly at risk 
# for overfitting.

# Note that you might have gotten a different answer than us, 
# because the glm function struggles with this many variables. 
# The warning messages that you might have seen in this problem 
# have to do with the number of variables, and the fact that the 
# model is overfitting to the training set. We'll discuss this 
# in more detail in the Homework Assignment.








