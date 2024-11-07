#Loading data

iris
data(iris)
summary(iris)


# Structure
str(iris)
#Installing Packages
install.packages("e1071") #Package to calculate the probabaility
#install.packages("caTools") #package to split the data

#Loading Package
library("e1071")
library("caTools")

# Splitting data into train
# and test data
split <- sample.split(iris, SplitRatio = 0.7)
split
train_cl <- subset(iris, split == "TRUE")
train_cl
test_cl <- subset(iris, split == "FALSE")
test_cl

# Feature Scaling 
train_scale <- scale(train_cl[, 1:4]) 
train_scale 
test_scale <- scale(test_cl[, 1:4]) 
test_scale
