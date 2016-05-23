#Set locale to USA
Sys.setlocale("LC_ALL", "C")

fp1 = file.path("C:","COURSERA","DATA SCIENCE - STANFORD - Statistical Learning")
fp2 = file.path("H:","COURSERA","INCOMING EDX & OTHER","DATA SCIENCE - STANFORD - Statistical Learning")

if (dir.exists(fp1)) {
  default_path = fp1
} else if (dir.exists(fp2)) {
  default_path = fp2
} else {
  print('Could not set default path')
}


# VECTORS

x = c(2,7,5)
x

# sequence function
?seq
seq(1,12)

# make a sequence vector
# from 4, length 3, +3
y = seq.int(from = 4,length.out = 3,by = 3)
y

x / y

x^y

# access elements of vector x (2 7 5)
x[2]
x[2:3]
x[-2]

# note: a scalar is just a vector of length 1


# MATRICES / ARRAYS

# numbers will be in column order
z = matrix(seq(1,12),4,3)
z

# access parts of a matrix
z[3:4,2:3]

# access first column, will switch to vector output
z[,1]

# access first column, keep matrix output
z[,1,drop=F]

# query dimension of matrix
dim(z)

# workding dir
ls()

# clean up working dir
rm(y)

# GENERATING DATA

# make 50 random uniforms
runif(50)

# make 50 random normals
rnorm(50)

# make a basic plot
plot(runif(50),rnorm(50),
     xlab = 'Random Uniform',
     ylab = 'Random Normal',
     col = 'blue',
     pch='*')

# make 2 plots in the plot window
par(mfrow=c(2,1))
plot(runif(50),rnorm(50))
hist(rnorm(50))

#reset view
par(mfrow=c(1,1))

# read in data and do basic analysis
auto = read.csv(file.path(default_path,"Auto.csv"))

head(auto)
dim(auto)
names(auto)
class(auto)
head(auto$horsepower,10) # horepower = categorical variable
summary(auto)  

plot(auto$cylinders , auto$mpg)

# way to skip declaring "auto$"
attach(auto)
search()
plot(cylinders,mpg)











