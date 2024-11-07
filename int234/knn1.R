data = read.csv(file.choose())  #prostate_cancer dataset
str(data)
summary(data)
is.na.data.frame(data)
data<-data[-1]
data<-data[-10]
table(data$diagnosis_result)


data$diagnosis_result<-factor(data$diagnosis_result,levels = c("B","M"),
                       labels = c("Benign","Maligant"))

round(prop.table(table(data$diagnosis_result)) * 100,
      digits = 1)
summary(data[c("radius","area","smoothness")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_n <- as.data.frame(lapply(data[2:9],
                               normalize))
summary(data_n$area)


data_train <- data_n[1:75, ] 
data_test <- data_n[76:100, ] 
data_train_labels <- data[1:75, 1] 
data_test_labels <- data[76:100, 1] 
data_train_labels 
data_test_labels


#install.packages("class")
library(class)
data_test_pred <- knn(train = data_train,
                      test = data_test,
                      cl = data_train_labels,
                      k = 6)
data_test_pred


#install.packages("gmodels")
library(gmodels)
CrossTable(x = data_test_labels,y = data_test_pred,prop.chisq = FALSE)

accuracy<-mean(data_test_pred == data_test_labels)
print(paste("Accuracy:",accuracy*100))

