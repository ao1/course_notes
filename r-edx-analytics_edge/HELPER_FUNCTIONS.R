#Set locale to USA
Sys.setlocale("LC_ALL", "C")

default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")


outcome_measures <- function (tn,fn,fp,tp) {

  #tn = True Negatives
  #fn = False Negatives
  #tp = False Positives
  #fp = True Positives
  
  #                    Predicted Class = 0    Predicted Class = 1
  # Actual Class=0              tn                    fp
  # Actual Class=1              fn                    tp    
  
  #                    Predicted Class = 0    Predicted Class = 1
  # Actual Class=0        True Negatives        False Positives
  # Actual Class=1        False Negatives       True Positives    

  cat("Model Accuracy: ", (tn+tp)/(tn+fn+fp+tp) , "\n")
  cat("Baseline Accuracy: ", (tn+fp)/(tn+fn+fp+tp) , "\n")  
  cat("Sensitivity: " , tp/(tp+fn), "\n" )
  cat("Specificity: " , tn/(tn+fp), "\n" )
  cat("False Negative Error Rate: " , fn/(tp+fn), "\n" )
  cat("False Positive Error Rate: " , fp/(tn+fp), "\n" )
}



