getwd()
#insurance=read.csv("insurance.csv",stringsAsFactors = FALSE)
insurance=read.csv(file.choose(),stringsAsFactors = FALSE)
View(insurance)
table(insurance$sex)
str(insurance)
summary(insurance$charges)
summary(insurance$smoker)
quantile(insurance$charges,seq(from=0,to=1,by=0.20))
table1=table(insurance$sex)
table1
n=prop.table(table1)*100
n
round(n)
min(insurance$charges)
max(insurance$charges)