#Set locale to USA
Sys.setlocale("LC_ALL", "C")


if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
  
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
  
} else {
  
  print('Could not set default path')
  
}

########
######## Read in and familiarize with data
########

#add stringsAsFactors because we are dealing with text data
emails = read.csv(file.path(default_path,"energy_bids.csv"),stringsAsFactors = FALSE)


str(emails)
#855 obs (labeled emails), for each one we have the text
#and whether or not it is responsive


#browse email contents
emails$email[1]
emails$responsive[1]

#wrap lines for long message
strwrap(emails$email[2])

#the data set is unbalanced, 0.1625731 are responsive emails
prop.table(table(emails$responsive))


########
######## Pre-process data
########

library(tm)

#pass in all emails to corpus variable
corpus = Corpus(VectorSource(emails$email))

#output 1st email in corpus
strwrap(corpus[[1]])

#switch to lowercase
corpus = tm_map(corpus , tolower)
corpus = tm_map(corpus, PlainTextDocument)

#remove punctuation marks
corpus = tm_map(corpus , removePunctuation)

#check length of stopwords vector
length(stopwords("english")) 

#note: if you want to remove words, use tm_map(corpus, removeWords, sw)

#remove stopwords
corpus = tm_map(corpus , removeWords , stopwords("english"))

#stem words
corpus = tm_map(corpus , stemDocument)


#check 1st email again to see if data is ready for machine learning
strwrap(corpus[[1]])


########
######## Create document term matrix and data frame
########

dtm = DocumentTermMatrix(corpus)

dtm

# <<DocumentTermMatrix (documents: 855, terms: 22164)>>
#   Non-/sparse entries: 102863/18847357
# Sparsity           : 99%
# Maximal term length: 156
# Weighting          : term frequency (tf)

#22164 words that showed up 1+ times, in 855 emails
#too many words, reduce the frequency to 3%

dtm = removeSparseTerms(dtm , 0.97)


dtm

# <<DocumentTermMatrix (documents: 855, terms: 788)>>
#   Non-/sparse entries: 51612/622128
# Sparsity           : 92%
# Maximal term length: 19
# Weighting          : term frequency (tf)


#make data frame
labeledTerms = as.data.frame(as.matrix(dtm))
#add outcome variable to data frame
labeledTerms$responsive = emails$responsive

str(labeledTerms)
#789 variables, 788 independent and last one is dependent


########
######## Create train and test set
########

library(caTools)

set.seed(144)

spl = sample.split(labeledTerms$responsive , 0.7)

train = subset(labeledTerms , spl == TRUE)
test = subset(labeledTerms , spl == FALSE)


########
######## Build a CART model
########

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~ . , data=train , method="class")

prp(emailCART)


########
######## Evaluate CART model on test set
########

pred = predict(emailCART , newdata = test)

#check first 10 predictions
pred[1:10,]   #or head(pred,n = 10)

#              prob of nonresp | prob of resp
#character(0)       0.2156863    0.78431373

#pred.prob = test set predicted probabilities
pred.prob = pred[,2]

#confusion matrix
table(test$responsive , pred.prob >= 0.5)

prop.table(table(test$responsive , pred.prob >= 0.5))

#accuracy = 0.8560311
sum(diag(table(test$responsive , pred.prob >= 0.5))) / sum(table(test$responsive , pred.prob >= 0.5))


########
######## Evaluate baseline model on test set
########

prop.table(table(test$responsive))

#baseline accuracy: 0.8365759

#CART model offers a small improvement in accuracy over the baseline


#assign a higher cost to false negatives

########
######## ROC curve and AUC value
########

library(ROCR)

predROCR = prediction(pred.prob , test$responsive )

perfROCR = performance( predROCR , "tpr"  , "fpr" )

plot(perfROCR , colorize = TRUE)

#we favor high sensitivity

performance(predROCR,"auc")@y.values
#test set AUC = 0.7936323
#model can differentiate about 80% of the time





