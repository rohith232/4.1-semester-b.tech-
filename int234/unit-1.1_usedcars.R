usedcars <- read.csv(file.choose(),
                     stringsAsFactors = FALSE)

View(usedcars)

str(usedcars)
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])
summary(usedcars$transmission)


range(usedcars$price)
diff(range(usedcars$price))


range(usedcars$mileage)
diff(range(usedcars$mileage))

range(usedcars$transmission)
diff(range(usedcars$transmission))


quantile(usedcars$price, seq(from = 0,
                             to = 1, by = 0.20))

quantile(usedcars$price, seq(from = 0,
                             to = 1, by = 0.10))
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)
table(usedcars$price)



model_table <- table(usedcars$model)
model_table

prop.table(model_table)




color_pct <- table(usedcars$color)
color_pct <- prop.table(color_pct) * 100
color_pct
round(color_pct, digits = 1)
head(usedcars)
tail(usedcars)
nrow(usedcars)
ncol(usedcars)