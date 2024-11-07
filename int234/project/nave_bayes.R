
########################### Naive Bayes Implementation ###########################################################
# Load the dataset
data <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Check the structure of the dataset
str(data)

# Assuming the last column is the class label, convert it to a factor
data$User.Behavior.Class <- factor(data$User.Behavior.Class)

# Normalizing the feature columns
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalize the dataset excluding the class label

# Select only numeric columns for normalization
numeric_columns <- sapply(data, is.numeric)
data_n <- as.data.frame(lapply(data[, numeric_columns], normalize))

# Summary of the normalized data
summary(data_n)

# Splitting the data into training and test sets
library(caTools)

set.seed(123)  # For reproducibility
split <- sample.split(data$User.Behavior.Class, SplitRatio = 0.7)
train_set <- subset(data_n, split == TRUE)
test_set <- subset(data_n, split == FALSE)
train_labels <- subset(data$User.Behavior.Class, split == TRUE)
test_labels <- subset(data$User.Behavior.Class, split == FALSE)

# Loading the required package for Naive Bayes
# install.packages("e1071")  # Uncomment this line if e1071 is not installed
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
print(paste("Accuracy Naive Bayes:", round(accuracy * 100, 2), "%"))


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
