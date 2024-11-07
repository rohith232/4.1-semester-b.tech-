# Load the Iris dataset
data(iris)
str(iris)

# Converting the Species column to a factor
iris$Species <- factor(iris$Species)

# Normalizing the feature columns
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


iris_n <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_n)

# Splitting the data into training and test sets
library(caTools)

set.seed(123)  # For reproducibility
split <- sample.split(iris$Species, SplitRatio = 0.7)
train_set <- subset(iris_n, split == TRUE)
test_set <- subset(iris_n, split == FALSE)
train_labels <- subset(iris$Species, split == TRUE)
test_labels <- subset(iris$Species, split == FALSE)

# Loading the required package
#install.packages("class")
library(class)

# Applying KNN algorithm
k_value <- 10  # You can adjust this value based on your requirement
predicted_labels <- knn(train = train_set, test = test_set, cl = train_labels, k = k_value)
predicted_labels

# Evaluating the model using a confusion matrix
#install.packages("gmodels")
library(gmodels)

CrossTable(x = test_labels, y = predicted_labels, prop.chisq = FALSE)

# Calculating accuracy manually
cm <- table(test_labels, predicted_labels)
accuracy <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy KNN:", round(accuracy * 100, 2), "%"))
