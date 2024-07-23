car_data = read.csv("/Users/Asus/Desktop/mh3511ford/ford.csv")
ori_copy = read.csv("/Users/Asus/Desktop/mh3511ford/ford.csv")
library(psych)
library(dplyr)

#data inspection
head(car_data)
car_price = car_data$price
summary(car_data$price)
hist(car_price , main = "Histogram of Price", xlab="price")
boxplot(car_price, main = "Boxplot of car_price")

#3.1.1 main variable of interest: price
#transforming car_data$price
log_car_price <- log(car_data$price)
hist(log_car_price, main = "Histogram of ln_price", xlab="ln_price")
boxplot(log_car_price, main = "Boxplot of ln_price", ylab="ln_price")

# Remove outliers
Q1 = quantile(log_car_price,0.25)
Q3 = quantile(log_car_price,0.75)
IQR = Q3-Q1
filtered_log_car_price = log_car_price[log_car_price >= Q1 -IQR*1.5 &  log_car_price <= Q3 + IQR*1.5]
hist(filtered_log_car_price, main = "Histogram of ln_price", xlab="ln_price")
boxplot(filtered_log_car_price, main = "Boxplot of ln_price", ylab="ln_price")
ln_price = filtered_log_car_price
summary(ln_price)
qqnorm(filtered_log_car_price)
qqline(filtered_log_car_price, col="blue")

# Add a ln_price column and eliminate outliers
filtered_car_data = car_data[log_car_price >= Q1 -IQR*1.5 &  log_car_price <= Q3 + IQR*1.5,]
filtered_car_data$ln_price = log(filtered_car_data$price)
head(filtered_car_data)
length(filtered_log_car_price)
length(filtered_car_data)
outliers_percentage = 1 - nrow(filtered_car_data)/nrow(car_data)
outliers_percentage
car_data = filtered_car_data

#3.2.1 model
str(car_data$model)
model_table <- aggregate(car_data$price,list(car_data$model),FUN=length)
colnames(model_table) <- c("Model","Count")
mode = model_table[model_table$Count == max(model_table$Count),]

par(mar = c(5, 9, 4, 2))  # Set margins: bottom, left, top, right
barplot(model_table$Count,
        names=model_table$Model,
        horiz = T,
        cex.names = 0.75,
        main="Number of Cars by Model (Before)",
        xlab="Count",
        las=2)

#after
models = model_table$Model[model_table$Count >= 200]
filtered = car_data[car_data$model %in% models ,]
model_table = aggregate(filtered$price, list(filtered$model), FUN=length)
colnames(model_table) = c("Model", "Count")
barplot(model_table$Count,
        names=model_table$Model,
        horiz = T,
        cex.names = 0.75,
        main="Number of Cars by Model (After)",
        xlab="Count",
        las=2)
nrow(car_data)-nrow(filtered)
1-nrow(filtered)/nrow(car_data)
car_data = filtered

#3.2.2 transmission
str(car_data)
transmission_table = aggregate(car_data$price, list(car_data$transmission), FUN=length)
colnames(transmission_table) = c("Transmission", "Count")
barplot(transmission_table$Count,
        names=transmission_table$Transmission,
        horiz = T,
        cex.names = 0.75,
        main="Cars by Transmission",
        xlab="Count",
        las=2)

#3.2.3 fuelType
str(car_data$fuelType)
fuel_type_table = aggregate(car_data$price, list(car_data$fuelType), FUN=length)
colnames(fuel_type_table) = c("Fuel_type", "Count")
fuel_type_table
barplot(fuel_type_table$Count,
        names=fuel_type_table$Fuel_type,
        horiz = T,
        cex.names = 0.75,
        main="Cars by fuelType (Before)",
        xlab="Count",
        las=2)

#after
filtered = car_data[car_data$fuelType == "Petrol" | car_data$fuelType == "Diesel",]
fuel_type_table = aggregate(filtered$price, list(filtered$fuelType), FUN=length)
colnames(fuel_type_table) = c("Fuel_type", "Count")
fuel_type_table
barplot(fuel_type_table$Count,
        names=fuel_type_table$Fuel_type,
        horiz = T,
        cex.names = 0.75,
        main="Cars by fuelType (After)",
        xlab="Count",
        las=2)
1-nrow(filtered)/nrow(car_data)
nrow(car_data)-nrow(filtered)
car_data=filtered
head(car_data)

#3.2.4 mileage
str(car_data$mileage)
hist(car_data$mileage, main="Histogram of mileage", xlab="mileage")
boxplot(car_data$mileage, main="Boxplot of mileage", ylab="mileage")
hist(log(car_data$mileage)^3, main="Histogram of (ln_mileage)^3", xlab="(ln_mileage)^3")
boxplot(log(car_data$mileage)^3, main="Boxplot of (ln_mileage)^3", ylab="(ln_mileage)^3")
sum(car_data$mileage==0)
sum(car_data$mileage<=1000)
car_data$ln_cube_mileage = log(car_data$mileage)^3
head(car_data)

#3.2.5 mpg
head(car_data)
str(car_data$mpg)
hist(car_data$mpg, main="Histogram of mpg", xlab="mpg")
boxplot(car_data$mpg, main="Boxplot of mpg", ylab="mpg")

# 3.2.6 year
str(car_data$year)
car_data = car_data[car_data$year != 2060,]
year_table = aggregate(car_data$year, list(car_data$year), FUN=length)
colnames(year_table) = c("Year", "Count")
year_table
barplot(year_table$Count,
        names=year_table$Year,
        horiz = T,
        cex.names = 0.75,
        main="Cars by Year (Before)",
        xlab="Count",
        las=2)
years = year_table$Year[year_table$Count >= 30]
filtered = car_data[car_data$year %in% years,]
nrow(car_data) - nrow(filtered)
1-nrow(filtered)/nrow(car_data)

year_table = aggregate(filtered$year, list(filtered$year), FUN=length)
colnames(year_table) = c("Year", "Count")
year_table
barplot(year_table$Count,
        names=year_table$Year,
        horiz = T,
        cex.names = 0.75,
        main="Cars by Year (After)",
        xlab="Count",
        las=2)
car_data = filtered

#4.1.1 ln_price vs model
ggplot(data = car_data, aes(x = model,y = ln_price), ) + ggtitle("Boxplot of ln_price against car models")  +geom_boxplot()
boxplot(car_data$ln_price~car_data$model, car_data, xlab="model", ylab="ln_price", main='Boxplot of ln_price against car models')
summary(aov(car_data$ln_price~factor(car_data$model)))
pairwise.t.test(car_data$ln_price, car_data$model, p.adjust.method="none") 

#4.1.2 ln_price vs transmission
boxplot(car_data$ln_price~car_data$transmission, car_data, xlab="Transmission", ylab="ln_price", main="Boxplot of ln_price against transmission")
aov(car_data$ln_price~factor(car_data$transmission))
summary(aov(car_data$ln_price~factor(car_data$transmission))) 
pairwise.t.test(car_data$ln_price, car_data$transmission, p.adjust.method="none") 

#4.1.3 ln_price vs fuelType
boxplot(car_data$ln_price~car_data$fuelType, car_data, xlab="fuelType", ylab="ln_price", main="Boxplot of ln_price against fuelType")
var.test(car_data[car_data$fuelType == "Diesel",10],car_data[car_data$fuelType == "Petrol",10])
t.test(car_data[car_data$fuelType == "Diesel",10],car_data[car_data$fuelType == "Petrol",10], var.equal = F)

#4.1.4 ln_price vs ln_cube_mileage
plot(car_data$ln_price~car_data$ln_cube_mileage, xlab="ln_cube_mileage", ylab="ln_price", main="Scatterplot of ln_price against ln_cube_mileage", pch=19, cex=0.5)
abline(lm(car_data$ln_price~car_data$ln_cube_mileage), col='red')
lmodel = lm(car_data$ln_price~car_data$ln_cube_mileage)
summary(lmodel)

#4.1.5 ln_price vs year
boxplot(car_data$ln_price~car_data$year, car_data, xlab="Year", ylab="ln_price", main='Boxplot of ln_price against year')
aov(car_data$ln_price~factor(car_data$year))
summary(aov(car_data$ln_price~factor(car_data$year))) 
pairwise.t.test(car_data$ln_price, car_data$year, p.adjust.method="none") 

#4.2.1 correlation matrix with scatterplots
pairs.panels(car_data[,c(10,11,8)],main='Correlation between ln_price with other continuous variables')

#4.2.2 single linear regression
model1 = lm(car_data$ln_price ~ car_data$ln_cube_mileage)
summary(model1)
qqnorm(model1$residuals, main='qqplot of residuals for ln_cube_mileage')
qqline(model1$residuals)
model2 = lm(car_data$ln_price ~ car_data$mpg)
summary(model2)
qqnorm(model2$residuals, main='qqplot of residuals for log_mpg')
qqline(model2$residuals)

#4.2.3 multiple linear regression
multipleModel = lm(car_data$ln_price~ car_data$mpg + car_data$ln_cube_mileage)
summary(multipleModel)
step(multipleModel, direction='backward')
