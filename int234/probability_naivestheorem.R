#Loading data

iris
data(iris)
summary(iris)


# Structure
str(iris)
#Installing Packages
#install.packages("e1071") #Package to calculate the probabaility
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


#train_c1[15:25,c("Sepal.Length")]

15:25
1:10

# Fitting Naive Bayes Model 
# to training dataset 

classifier_cl <- naiveBayes(Species ~ ., 
                            data = train_cl) 
classifier_cl


# Predicting on test data' 
y_pred <- predict(classifier_cl,  
                  newdata = test_cl) 
y_pred 
# Confusion Matrix 
cm <- table(test_cl$Species, y_pred) 
cm


#accuracy = (TP + TN) / (TP + TN + FP + FN)

acc<- (20+20+18)/(20+20+2+18)
acc


# Using paste()
print(paste("Accuracy:", acc * 100))

# Using sprintf()
print(sprintf("Accuracy: %.2f", acc * 100))

