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

# 2. Variable Importance Plot (Note: SVM doesn't directly provide feature importance)
# One way to estimate feature importance is by using the weights from the linear kernel SVM
# For a linear kernel, coefficients can be accessed using svm_model$coefs

# Extracting coefficients for linear kernel (if using a linear kernel)
if (svm_model$type == "C-classification" && svm_model$kernel == "linear") {
  coef_matrix <- as.matrix(svm_model$coefs)
  feature_importance <- abs(coef_matrix)
  feature_importance_df <- data.frame(Feature = colnames(trainData)[-which(names(trainData) == "User.Behavior.Class")], 
                                      Importance = feature_importance)
  
  # Sorting by importance
  feature_importance_df <- feature_importance_df[order(feature_importance_df$Importance, decreasing = TRUE), ]
  
  # Plot feature importance
  ggplot(feature_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) + 
    geom_bar(stat = "identity", fill = "blue") + 
    coord_flip() + 
    labs(title = "Feature Importance for SVM (Linear Kernel)", x = "Feature", y = "Importance") +
    theme_minimal()
} else {
  cat("Feature importance cannot be directly computed for non-linear SVM kernels.\n")
}

