Sys.setlocale("LC_ALL", "C")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
} else {
  next
}

# Add the argument stringsAsFactors=FALSE since we have some text fields
eBayTrain = read.csv(file.path(default_path,"eBayiPadTrain.csv"), stringsAsFactors=FALSE)
eBayTest = read.csv(file.path(default_path,"eBayiPadTest.csv"), stringsAsFactors=FALSE)

library(caTools)
spl = sample.split(eBayTrain$sold, SplitRatio = 0.7)
Train = subset(eBayTrain, spl==TRUE)
Test = subset(eBayTrain, spl==FALSE)

#  description: chr  
#  biddable   : int  
#  startprice : num
#  condition  : chr
#  cellular   : chr
#  carrier    : chr
#  color      : chr
#  storage    : chr
#  productline: chr
#  sold       : int
#  UniqueID   : int




#MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest2prob)
#write.csv(MySubmission, file.path(substr(default_path,0,51),"Submission2.csv"), row.names=FALSE)
