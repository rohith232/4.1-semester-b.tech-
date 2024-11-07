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


