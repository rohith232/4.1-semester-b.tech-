sales=read.csv(file.choose())

summary(sales)
is.na(sales)

#checking the null values in columns
sum(is.na(sales$Order.Priority))


#there are a lot of missing values in columns such as order.Priority
#Profit, Unit.Price and Customer.Name



sales<-na.omit(sales)#removing rows containing missing values



####dataset===sales

#name
#Replace the missing values with the mean value of each variable
dataset$Sales[is.na(dataset$Sales)] <-
  mean(dataset$Sales, na.rm = TRUE)
dataset$Profit[is.na(dataset$Profit)] <- mean(dataset$Profit, na.rm = TRUE)
dataset$Unit.Price[is.na(dataset$Unit.Price)] <- mean(dataset$Unit.Price, na.rm = TRUE)
summary(dataset)
