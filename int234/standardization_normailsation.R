#age vector
age <- c(19,20,21,22,23,24,24,26,27)
#Salary vec((tor
salary <- c(10000,20000,30000,40000,50000,60000,70000,80000,90000)

#data frame created using age and salary

df<- data.frame("Age"=age,
                "Salary"=salary,
                stringsAsFactors = FALSE)

df


#standardization

data<-as.data.frame(sapply(df,function(x)
  (x-mean(x))/sd(x)))

data

#Normalisation values will be always between 0 and 1

data2<-as.data.frame(sapply(df,function(x)
  (x-min(x))/(max(x)-min(x))))

data2


#another example
#rnorm is used to generate a series of random numbers(Normal distribution)
#rnorm(n,mean,standard deviation)
#by default mean is 0 and SD is 1
rnorm(5)
rnorm(10,2,3)


data3 <- data.frame(Age = rnorm(500, 50, 8), 
                    Weight = rnorm(500, 80, 10)) 
data3 

data4 <- as.data.frame(sapply(data3, function(x)  
  (x-mean(x))/sd(x))) 
data4 
#normalization 
data5 <- as.data.frame(sapply(data3, function(x)  
  (x-min(x))/(max(x)-min(x))))
data5