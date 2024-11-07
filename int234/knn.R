getwd()
wbcd<-read.csv(file.choose(),stringsAsFactors = FALSE) #"C:\Users\Dell\Downloads\wisc_bc_data.csv"

str(wbcd)
wbcd<-wbcd[-1] #removing the first col- id
table(wbcd$diagnosis)

wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c("B","M"),
                       labels = c("Benign","Maligant"))

round(prop.table(table(wbcd$diagnosis)) * 100,
      digits = 1)

summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31],
                               normalize))
summary(wbcd_n$area_mean)


wbcd_train <- wbcd_n[1:469, ] 
wbcd_test <- wbcd_n[470:569, ] 
wbcd_train_labels <- wbcd[1:469, 1] 
wbcd_test_labels <- wbcd[470:569, 1] 
wbcd_train_labels 
wbcd_test_labels


#install.packages("class")
library(class)
wbcd_test_pred <- knn(train = wbcd_train,
                      test = wbcd_test,
                      cl = wbcd_train_labels,
                      k = 23)
wbcd_test_pred


#install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_labels,y = wbcd_test_pred)



####################KNNKNNKNNKNN KNN ######################################################

# Set the working directory
getwd()

# Load the dataset
data <- read.csv(file.choose(), stringsAsFactors = FALSE)  # Replace with your file path if needed

# View the structure of the dataset
str(data)

# Convert User.Behavior.Class  to a factor
data$User.Behavior.Class  <- factor(data$User.Behavior.Class )

# Check the distribution of the User.Behavior.Class  variable
table(data$User.Behavior.Class )

# Display the proportion of each class
round(prop.table(table(data$User.Behavior.Class )) * 100, digits = 1)

# Normalize the features (assuming the features are in columns 2 to n)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_n <- as.data.frame(lapply(data[, -1], normalize))  # Exclude the class column for normalization

# Summary of normalized features
summary(data_n)

# Split the dataset into training and testing sets (70% train, 30% test)
set.seed(123)  # Set seed for reproducibility
train_index <- sample(1:nrow(data_n), 0.7 * nrow(data_n))
data_train <- data_n[train_index, ]
data_test <- data_n[-train_index, ]

# Store the labels for training and testing sets
train_labels <- data$User.Behavior.Class [train_index]
test_labels <- data$User.Behavior.Class [-train_index]

# Load the class package for k-NN
library(class)

# Make predictions using k-NN
k <- 23  # You can adjust this value based on your dataset
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


