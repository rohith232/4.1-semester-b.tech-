#salesdata.csvfile
dataset<-read.csv(file.choose(),stringsAsFactors = TRUE)
str(dataset)
summary(dataset)

dataset[!complete.cases(dataset),]
nrow(dataset[!complete.cases(dataset),])


#1st method
dataset$Unit.Price = ifelse(is.na(dataset$Unit.Price),
                            ave(dataset$Unit.Price,
                                FUN = function(x) mean(x, na.rm = TRUE)),
                            dataset$Unit.Price)

nrow(dataset[!complete.cases(dataset),])
#2nd Method
dataset$Sales[is.na(dataset$Sales)] <-
  mean(dataset$Sales, na.rm = TRUE)

nrow(dataset[!complete.cases(dataset),])

#there are lot of missing values in columns such as Order.Priority, Sales, Ship.Mode,
#Profit, Unit.Price and Customer.Name
dataset1 <- na.omit(dataset) #removing rows containing missing values and giving it a new


#replace categorical missing values by random value from each variable 
dataset$Order.Priority[is.na(dataset$Order.Priority)] <-  
  sample(levels(dataset$Order.Priority), 
         size = sum(is.na(dataset$Order.Priority)), 
         replace = TRUE) 
nrow(dataset[!complete.cases(dataset),])

sum(is.na(dataset))
