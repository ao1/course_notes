#Set locale to USA
Sys.setlocale("LC_ALL", "C")

require("caTools","rpart","rpart.plot")

if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
  
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
  
} else {
  
  print('Could not set default path')
  
}


#===========================================
# Flower
#===========================================


#read in data
flower = read.csv(file.path(default_path,"flower.csv"),header=FALSE)

#covert df -> matrix
flowerMatrix = as.matrix(flower)

#convert matrix -> df
flowerVector = as.vector(flowerMatrix)

#compute distance 
distance = dist(flowerVector, method="euclidian")

#Hierarchical Cluster intensity
clusterIntensity = hclust(distance,method = "ward.D")

# dendrogram overview
#
# lowest nodes = obsrevations/data
# higher nodes = clusters
# vertical lines = distance between 2 clusters/nodes
# the taller the line between nodes, the more dissimilar the clusters are 
# for each imaginary cut line, read clusters from the bottom
# less clusters = more coarse clustering, but having too many can be bad too
# good cut = leaves room to move up and down
# 

plot(clusterIntensity)

#2-3 clusters is optimal here
#add red rectangles to see the clusters more clearl on the plot
rect.hclust(clusterIntensity , k=3 , border = "red")

#actually split the dendrogram into 3 clusters
flowerClusters = cutree(clusterIntensity , k=3)

#vector of values
flowerClusters

#find mean intensity by applying mean to flowerVector, according to flowerClusters
tapply(flowerVector,flowerClusters,mean)

#switch flowerClusters to a 2 dimensional (50 by 50) type
dim(flowerClusters) = c(50,50)

#show image
image(flowerClusters,axes=FALSE)
#darkest shade = cluster1

#original image
image(flowerMatrix,axes=FALSE,col = grey(seq(0,1,length=256)))

#===========================================
# MRI
#===========================================


#read in data , intensoty values = header must be set to false
healthy = read.csv(file.path(default_path,"healthy.csv"),header=FALSE)

#make matrix and vector objects
healthyMatrix = as.matrix(healthy)
healthyVector = as.vector(healthyMatrix)

str(healthyMatrix)
#we have a 566 by 646 resolution

#make greyscale image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

#compute the distance matrix
#distance = dist(healthyVector,method="euclidian")  <- this fails, too much memory
str(healthyVector)
#healthyVector has 365636 elements
n = 365636
n*(n-1)/2
#we would need 66844659430 bytes for storing in a matrix to do this operation


#We cannot use hierarhical clustering, but we can use k-means clustering instead

set.seed(1)
k=5

#kmeans(what_we_want_to_cluster, centers=cluster_count , iter.max=max_iterations)
KMC = kmeans(healthyVector,centers = k,iter.max = 1000)

str(KMC)

#extract cluster categorization into a vector variable
healthyClusters = KMC$cluster

#mean intensity of all clusters, all are closet to 0, so image is dark
KMC$centers

#sizes of clusters
str(KMC)
#centers     : num [1:5, 1] 0.4818 0.1062 0.3094 0.1842 0.0196
#size        : int [1:5]     20556 101085 31555  79278  133162


#output segmented image with rainbow colors, 1 for each cluster
dim(healthyClusters) = c( nrow(healthyMatrix) , ncol(healthyMatrix))

image(healthyClusters , axes=FALSE , col=rainbow(k))

#===========================================
# Extra
#===========================================


KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000)

KMC2$withinss

NumClusters = seq(2,10,1)

SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))

plot(NumClusters, SumWithinss, type="b")

#sapply info: http://www.r-bloggers.com/using-apply-sapply-lapply-in-r/

#===========================================
# MRI Part 2
#===========================================

#read in tumor (testing) data
tumor = read.csv(file.path(default_path,"tumor.csv"),header=FALSE)

#covert df -> matrix
tumorMatrix = as.matrix(tumor)

#convert matrix -> df
tumorVector = as.vector(tumorMatrix)


#we apply the k-means clustering result from above on tumor data

#install.packages("flexclust")
library(flexclust)

#convert to kcca object (will take some time)
KMC.kcca = as.kcca(KMC, healthyVector)

#cluster the pixels in the tumor vector using the predict function
tumorClusters = predict(KMC.kcca , newdata = tumorVector)

#tumorClusters is a vector, need to conver to matrix
dim(tumorClusters) = c(nrow(tumorMatrix),ncol(tumorMatrix))

#identify the geometry of tumor
image(tumorClusters,axes=FALSE,col=rainbow(k))

#better method to try: modified fuzzy k-means clustering method
#can do 3d reconstruction from 2d cross sectional mri images


#===========================================
# Method comparison
#===========================================

# Linear Regression
# 
# Predicts: Salary, Price, number of votes, etc.
# 
# Pros: Simple, well recognized. Works on small and large datasets.
#   
# Cons: Assumes a linear relationship.


# Logistic Regression
# 
# Predicts: Yes/No , Sell/Buy , Accept/Reject , etc.
# 
# Pros: Computes probabilities that can be used to assess 
# confidence of prediction
# 
# Cons: Assumes a linear relationship.
  

# CART
# 
# Predicts: Quality rating 1-5 , Buy/Sell/Hold or continious 
# outcome (saraly, price)
#   
# Pros: Can handle datasets without a linear relationship
#   
# Cons: May not work well with small datasets
  
  
  
# Random Forest  
#   
# Predicts: Quality rating 1-5 , Buy/Sell/Hold or continious outcome (saraly, price)
#   
# Pros: Can improve accuracy over CART
#   
# Cons: Many parameters to adjust, not as easy to explain as CART
   
  
# Hierarchical Clustering
# 
# Predicts: Finding similar groups. Clustering into smaller 
# groups and applying predictive methods on groups
#   
# Pros: No need to select number of clusters in advance. Visualize with a dendrogram
# 
# Cons: Hard to use with large datasets


# k-means clustering
# 
# Predicts: Finding similar groups. Clustering into smaller 
# groups and applying predictive methods on groups
# 
# Pros: Works with datasets of any size
#   
# Cons: Need to select the number of clusters before running algorithm
  
  
  
  
  
  
  