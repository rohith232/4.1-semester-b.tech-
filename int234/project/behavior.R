#Mobile Device Usage and User Behavior Dataset

# Load necessary libraries
library(randomForest)
library(caret)  # For cross-validation and better model evaluation

# Read the dataset
data <- read.csv(file.choose())

# View the dataset (optional)
View(data)
colnames(data)
# Ensure 'User.Behavior.Class' is a factor
data$User.Behavior.Class <- as.factor(data$User.Behavior.Class)

# Check the structure of the dataset
str(data)

# Remove any columns that are lists or non-atomic
data <- data[sapply(data, is.atomic)]

# Split the dataset into training and testing sets (70% train, 30% test)
set.seed(123)  # For reproducibility
train_indices <- createDataPartition(data$User.Behavior.Class, p = 0.7, list = FALSE)
trainData <- data[train_indices, ]
testData <- data[-train_indices, ]

# Fit the Random Forest model with hyperparameter tuning
rf_model <- randomForest(User.Behavior.Class ~ ., data = trainData, ntree = 500, mtry = 3, importance = TRUE)

# Print the model summary
print(rf_model)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = testData)

# Evaluate the model
confusion_matrix <- table(testData$User.Behavior.Class, predictions)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy * 100, "%\n")  # Display accuracy as a percentage

# ---- Visualization ----

# 1. Confusion Matrix Plot
# Convert confusion matrix to a dataframe for ggplot
confusion_matrix_df <- as.data.frame(confusion_matrix)
colnames(confusion_matrix_df) <- c("Actual", "Predicted", "Count")

# Plot the confusion matrix
ggplot(data = confusion_matrix_df, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Count), vjust = 1) +
  labs(title = "Confusion Matrix for Random Forest", x = "Actual", y = "Predicted") +
  theme_minimal()

# 2. Variable Importance Plot
# Plotting variable importance
varImpPlot(rf_model, main = "Variable Importance for Random Forest")


############################################ KNN 2

# Set the working directory
getwd()

# Load the dataset
data <- read.csv(file.choose(), stringsAsFactors = FALSE)  # Replace with your file path if needed

# Convert User.Behavior.Class to a factor
data$User.Behavior.Class <- factor(data$User.Behavior.Class)

# Normalize the features (only numeric columns)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Select only numeric columns for normalization
numeric_columns <- sapply(data, is.numeric)
data_n <- as.data.frame(lapply(data[, numeric_columns], normalize))

# Add the User.Behavior.Class column back to the normalized data
data_n$User.Behavior.Class <- data$User.Behavior.Class

# Introduce noise to the numeric features
set.seed(123)  # For reproducibility
noise <- matrix(rnorm(n = nrow(data_n) * (ncol(data_n) - 1), mean = 0, sd = 0.1), nrow = nrow(data_n))
data_n[, -ncol(data_n)] <- data_n[, -ncol(data_n)] + noise  # Add noise to features

# Summary of data with noise
summary(data_n)

# Split the dataset into training and testing sets (70% train, 30% test)
set.seed(123)  # Set seed for reproducibility
train_index <- sample(1:nrow(data_n), 0.7 * nrow(data_n))
data_train <- data_n[train_index, ]
data_test <- data_n[-train_index, ]

# Store the labels for training and testing sets
train_labels <- data_n$User.Behavior.Class[train_index]
test_labels <- data_n$User.Behavior.Class[-train_index]

# Load the class package for k-NN
library(class)

# Increase the value of k to 50 (example, adjust as needed)
k <- 50
test_pred <- knn(train = data_train[, -ncol(data_train)],  # Exclude class column for training
                 test = data_test[, -ncol(data_test)],      # Exclude class column for testing
                 cl = train_labels,
                 k = k)

# Create a cross-tabulation of actual vs predicted values
library(gmodels)
CrossTable(x = test_labels, y = test_pred)

# Calculate accuracy
accuracy <- sum(test_pred == test_labels) / length(test_labels)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")

# Assuming 'predicted_labels' and 'test_labels' are available from the KNN model
library(ggplot2)
library(reshape2)

# Confusion Matrix for KNN
knn_cm <- table(test_labels, predicted_labels)

# Convert to a dataframe for ggplot
knn_cm_df <- as.data.frame(knn_cm)
colnames(knn_cm_df) <- c("Actual", "Predicted", "Count")

# Plot the confusion matrix
ggplot(data = knn_cm_df, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Count), vjust = 1) +
  labs(title = "Confusion Matrix for KNN", x = "Actual", y = "Predicted") +
  theme_minimal()





#############################Naive Bayes 2 ###############################################
# Load the dataset
data <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Check the structure of the dataset
str(data)

# Assuming the last column is the class label, convert it to a factor
data$User.Behavior.Class <- factor(data$User.Behavior.Class)

# Normalize the dataset (using all features for normalization)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalize the dataset excluding the class label
numeric_columns <- sapply(data, is.numeric)
data_n <- as.data.frame(lapply(data[, numeric_columns], normalize))


# Summary of the normalized data
summary(data_n)

# Reducing the features: Selecting only a few irrelevant or less informative features
# For demonstration, let's assume we only use the first two features
# This may not be suitable for your actual data, adjust as necessary
data_reduced <- data_n[, 1:2]  # Adjust according to the actual feature indices

# Splitting the data into training and test sets
library(caTools)

set.seed(123)  # For reproducibility
split <- sample.split(data$User.Behavior.Class, SplitRatio = 0.7)
train_set <- subset(data_reduced, split == TRUE)
test_set <- subset(data_reduced, split == FALSE)
train_labels <- subset(data$User.Behavior.Class, split == TRUE)
test_labels <- subset(data$User.Behavior.Class, split == FALSE)

# Loading the required package for Naive Bayes
library(e1071)

# Applying Naive Bayes algorithm
naive_bayes_model <- naiveBayes(train_set, train_labels)

# Making predictions on the test set
predicted_labels <- predict(naive_bayes_model, test_set)

# Evaluating the model using a confusion matrix
library(gmodels)

CrossTable(x = test_labels, y = predicted_labels, prop.chisq = FALSE)

# Calculating accuracy manually
cm <- table(test_labels, predicted_labels)
accuracy <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy Naive Bayes with reduced features:", round(accuracy * 100, 2), "%"))

# Assuming 'predicted_labels' and 'test_labels' are available from the Naive Bayes model
nb_cm <- table(test_labels, predicted_labels)

# Convert to a dataframe for ggplot
nb_cm_df <- as.data.frame(nb_cm)
colnames(nb_cm_df) <- c("Actual", "Predicted", "Count")

# Plot the confusion matrix
ggplot(data = nb_cm_df, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Count), vjust = 1) +
  labs(title = "Confusion Matrix for Naive Bayes", x = "Actual", y = "Predicted") +
  theme_minimal()













########################################################## COMPARING ################################
# Load necessary libraries
library(ggplot2)

# Assuming you have calculated the accuracies for each model
# Replace these values with the actual accuracies from your models
accuracies <- data.frame(
  Model = c("KNN", "Naive Bayes", "Random Forest"),
  Accuracy = c(0.85, 0.80, 0.90)  # Example accuracy values; replace with actual values
)

# Plotting the accuracies of the models
ggplot(data = accuracies, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Accuracy * 100, 2), "%")), 
            vjust = -0.5, size = 5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy") +
  theme_minimal() +
  theme(legend.position = "none")




################################## SVM ##################################################


# Load necessary libraries
library(e1071)  # For SVM
library(caret)  # For cross-validation and better model evaluation
library(ggplot2)  # For visualizations

# Read the dataset
data <- read.csv(file.choose())  # Load your dataset
View(data)  # View the dataset

# Ensure 'User.Behavior.Class' is a factor
data$User.Behavior.Class <- as.factor(data$User.Behavior.Class)

# Check the structure of the dataset
str(data)

# Remove any columns that are lists or non-atomic
data <- data[sapply(data, is.atomic)]

# Split the dataset into training and testing sets (70% train, 30% test)
set.seed(123)  # For reproducibility
train_indices <- createDataPartition(data$User.Behavior.Class, p = 0.7, list = FALSE)
trainData <- data[train_indices, ]
testData <- data[-train_indices, ]

# Fit the SVM model
svm_model <- svm(User.Behavior.Class ~ ., data = trainData, kernel = "radial", cost = 1, scale = TRUE)

# Print the model summary
print(svm_model)

# Make predictions on the test set
predictions <- predict(svm_model, newdata = testData)

# Evaluate the model (confusion matrix)
confusion_matrix <- table(testData$User.Behavior.Class, predictions)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy * 100, "%\n")  # Display accuracy as a percentage

# ---- Visualization ----

# 1. Confusion Matrix Plot
# Convert confusion matrix to a dataframe for ggplot
confusion_matrix_df <- as.data.frame(confusion_matrix)
colnames(confusion_matrix_df) <- c("Actual", "Predicted", "Count")

# Plot the confusion matrix
ggplot(data = confusion_matrix_df, aes(x = Actual, y = Predicted)) + 
  geom_tile(aes(fill = Count), color = "white") + 
  scale_fill_gradient(low = "white", high = "blue") + 
  geom_text(aes(label = Count), vjust = 1) + 
  labs(title = "Confusion Matrix for SVM", x = "Actual", y = "Predicted") + 
  theme_minimal()














