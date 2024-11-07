library(datasets)
library(caTools)
library(party)
library(dplyr)

data("readingSkills")
head(readingSkills)

sample_data =sample.split(readingSkills,SplitRatio = 0.8)
train_data <- subset(readingSkills,sample_data ==TRUE)
test_data <- subset(readingSkills,sample_data == FALSE)

model <- ctree(nativeSpeaker ~ ., train_data)
plot(model)

#testing the people who are native speakers
#and those who are not
predict_model <-predict(model,test_data)


#creates a table to count how many are classified 
#as native speakers and how many are not

m_at <-table(test_data$nativeSpeaker,predict_model)
m_at

ac_Test <-sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be',ac_Test))




#compare the accuracy of NAive Bayes, KNN on this dataset

#Naive Bayes theorem

library(datasets)
library(caTools)
library(e1071) # Library for Naive Bayes

data("readingSkills")
head(readingSkills)

# Split the data into training and testing sets
set.seed(123)
sample_data <- sample.split(readingSkills$nativeSpeaker, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)

# Train the Naive Bayes model
model <- naiveBayes(nativeSpeaker ~ ., data = train_data)

# Predict on the test data
predict_model <- predict(model, newdata = test_data)

# Create a confusion matrix
m_at <- table(test_data$nativeSpeaker, predict_model)
print(m_at)

# Calculate the accuracy
ac_Test <- sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', ac_Test * 100, "%"))

# Optional: Format the accuracy output
print(sprintf("Accuracy: %.2f%%", ac_Test * 100))



##KNN

# Load necessary libraries
library(caTools)
library(class)
library(gmodels)

# Load the readingSkills dataset
data("readingSkills")
head(readingSkills)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
sample_data <- sample.split(readingSkills, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)

# Normalize the continuous variables
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to relevant columns
train_data_n <- as.data.frame(lapply(train_data[2:4], normalize))
test_data_n <- as.data.frame(lapply(test_data[2:4], normalize))

# Use nativeSpeaker as the label
train_labels <- train_data$nativeSpeaker
test_labels <- test_data$nativeSpeaker

# Apply the KNN algorithm
k <- 4  # You can choose the value of k
predictions <- knn(train = train_data_n, test = test_data_n, cl = train_labels, k = k)

# Evaluate the model
CrossTable(x = test_labels, y = predictions, prop.chisq = FALSE)

# Calculate the accuracy
accuracy <- mean(predictions == test_labels)
print(paste("Accuracy:", accuracy * 100))


############################################################KNN

# Load necessary libraries
library(datasets)
library(caTools)
library(class)
library(gmodels)

# Load the readingSkills dataset
data("readingSkills")
head(readingSkills)

# Split the data into training and testing sets
set.seed(123)
sample_data <- sample.split(readingSkills, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)

# Normalize the continuous variables
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_data_n <- as.data.frame(lapply(train_data[2:4], normalize))
test_data_n <- as.data.frame(lapply(test_data[2:4], normalize))

# Use nativeSpeaker as the label
train_labels <- train_data$nativeSpeaker
test_labels <- test_data$nativeSpeaker

# Apply the KNN algorithm
k <- 3
predictions <- knn(train = train_data_n, test = test_data_n, cl = train_labels, k = k)

# Evaluate the model
CrossTable(x = test_labels, y = predictions, prop.chisq = FALSE)

# Calculate the accuracy
accuracy <- mean(predictions == test_labels)
print(paste("Accuracy:", accuracy * 100))



##Using SQL commands

library(sqldf)
sqldf("select * from train_data")
sqldf("select * from train_data where score >34")


