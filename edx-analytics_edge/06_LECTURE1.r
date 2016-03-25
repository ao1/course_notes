#Set locale to USA
Sys.setlocale("LC_ALL", "C")


if (dir.exists(file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - EDX - The Analytics Edge","DATA")    
  
} else if (dir.exists(file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA"))) {
  
  default_path = file.path("C:","COURSERA","DATA SCIENCE - EDX - The Analytics Edge","DATA")
  
} else {
  
  print('Could not set default path')
  
}


movies = read.table(file.path(default_path,"movielens.txt"), header=FALSE, sep="|",quote="\"")
str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)

table(movies$Western)
table(movies$Comedy)
table(movies$Romance & movies$Drama)


# Compute distances between movies[2:20]
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward.D") 

# Plot the dendrogram, 3 or 4 clusters would be a good number of clusters
# but we want more clusters for movie genres, 10 might be better
# If you want a lot of clusters, use another way to decide
plot(clusterMovies)

# Assign points to clusters, use 10 clusters
clusterGroups = cutree(clusterMovies, k = 10)

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage of movies 
#in each genre and cluster

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
#MIB goes into 2nd of 10 clusters

#create a new dataset with just cluster 2 movies
cluster2 = subset(movies, clusterGroups == 2)

cluster2$Title[1:10]

#You can repeat this for each cluster by changing 
#the clusterGroups number. However, if you also have 
#a lot of clusters, this approach is not that much 
#more efficient than just using the tapply function.


#Run the cutree function again to create the cluster groups, 
#but this time pick k = 2 clusters
clusterGroups2 = cutree(clusterMovies, k = 2)
spl = split(movies[2:20], clusterGroups2)
lapply(spl, colMeans)





#A more advanced approach uses the "split" and "lapply" 
#functions. The following command will split the data 
#into subsets based on the clusters:
  
spl = split(movies[2:20], clusterGroups)

# Then you can use spl to access the different clusters, because

spl[[1]]

# is the same as

subset(movies[2:20], clusterGroups == 1)

# so colMeans(spl[[1]]) will output the centroid of cluster 1. 
# But an even easier approach uses the lapply function. 
# The following command will output the cluster centroids for all clusters:
  
lapply(spl, colMeans)

# The lapply function runs the second argument (colMeans) 
# on each element of the first argument (each cluster 
# subset in spl). So instead of using 19 tapply commands, 
# or 10 colMeans commands, we can output our centroids 
# with just two commands: one to define spl, and then 
# the lapply command.

# Note that if you have a variable called "split" 
# in your current R session, you will need to remove it 
# with rm(split) so that you can use the split function.




