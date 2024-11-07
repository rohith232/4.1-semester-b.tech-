diabetes_data <- read.csv(file.choose(), stringsAsFactors = FALSE)

is.na.data.frame(diabetes_data)
diabetes_data<-na.omit(diabetes_data)


str(diabetes_data)
summary(diabetes_data)

diabetes_data <- na.omit(diabetes_data)


table(diabetes_data$Outcome)

diabetes_data$Outcome <- factor(diabetes_data$Outcome, 
                                levels = c(0, 1),
                                labels = c("No Diabetes", "Diabetes"))

round(prop.table(table(diabetes_data$Outcome)) * 100, digits = 1)

summary(diabetes_data[c("Pregnancies", "Glucose", "Insulin")])  

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

diabetes_normalized <- as.data.frame(lapply(diabetes_data[1:8], normalize))

summary(diabetes_normalized$Glucose)

train_data <- diabetes_normalized[1:75, ]  
test_data <- diabetes_normalized[76:100, ]

train_labels <- diabetes_data[1:75, 9]  
test_labels <- diabetes_data[76:100, 9]

train_labels
test_labels


library(class)

k_value <- 6
test_predictions <- knn(train = train_data, 
                        test = test_data, 
                        cl = train_labels, 
                        k = k_value)

test_predictions

library(gmodels)

CrossTable(x = test_labels, y = test_predictions, prop.chisq = FALSE)

accuracy <- mean(test_predictions == test_labels)
print(paste("Accuracy:", accuracy * 100))





library(sqldf)
data(iris)
str(iris)


# 1
max_petal_length <- max(iris$Petal.Length)

print(paste("Maximum Petal Length:", max_petal_length))





# 2
avg_sepal_length <- aggregate(Sepal.Length ~ Species, data = iris, FUN = mean)

print("Average Sepal Length for each Species:")
print(avg_sepal_length)

# 3
result_setosa <- sqldf("SELECT * FROM iris WHERE Species = 'setosa'")
print("Rows where Species is 'setosa':")
print(result_setosa)

