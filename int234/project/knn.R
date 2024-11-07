
######################################### KNN #####################################################
# Set the working directory
getwd()

# Load the dataset
data <- read.csv(file.choose(), stringsAsFactors = FALSE)  # Replace with your file path if needed

# View the structure of the dataset
str(data)

# Convert User.Behavior.Class to a factor
data$User.Behavior.Class <- factor(data$User.Behavior.Class)

# Check the distribution of the User.Behavior.Class variable
table(data$User.Behavior.Class)

# Display the proportion of each class
round(prop.table(table(data$User.Behavior.Class)) * 100, digits = 1)

# Normalize the features (assuming the features are in columns 2 to n)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Select only numeric columns for normalization
numeric_columns <- sapply(data, is.numeric)
data_n <- as.data.frame(lapply(data[, numeric_columns], normalize))  # Normalize only numeric columns

# Summary of normalized features
summary(data_n)

# Split the dataset into training and testing sets (70% train, 30% test)
set.seed(123)  # Set seed for reproducibility
train_index <- sample(1:nrow(data_n), 0.7 * nrow(data_n))
data_train <- data_n[train_index, ]
data_test <- data_n[-train_index, ]

# Store the labels for training and testing sets
train_labels <- data$User.Behavior.Class[train_index]
test_labels <- data$User.Behavior.Class[-train_index]

# Load the class package for k-NN
library(class)

# Make predictions using k-NN
k <- 50  # You can adjust this value based on your dataset
test_pred <- knn(train = data_train, test = data_test, cl = train_labels, k = k)

# Display the predictions
test_pred

# Load the gmodels package for confusion matrix
library(gmodels)

# Create a cross-tabulation of actual vs predicted values
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