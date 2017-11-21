#Multiple Linear Regression
getwd()

data("Orange")
str(Orange)
head(Orange)
tail(Orange)
View(Orange)

results <- lm(age ~ circumference, Orange)
results
summary(results)
pairs(Orange, col = rainbow(3))

results2 <- lm(age ~ circumference + Tree, Orange)
results2
summary(results2)

#Calculating the Analysis of Variance
anova(results, results2)

#Prediction based on our Models
table(Orange$age)
predict(results, data.frame(circumference = 100), interval = 'confidence')

#***************************************************************************************
#Multilinear Regression to predit the housing price in Sacramento, CA
library(caret)
data('Sacramento')
View(Sacramento)
str(Sacramento)
names(Sacramento)

#Prep the linear model
housing_price <- lm(price ~ beds+baths+sqft+type, Sacramento)
housing_price
housing_price$coefficients
summary(housing_price)
#Prep another linear model
housing_price2 <- lm(price ~ zip+beds+sqft+type+latitude+longitude, Sacramento)
housing_price2
summary(housing_price2)
#Prep another linear model
housing_price3 <- lm(price ~ zip+beds+sqft+type, Sacramento)
housing_price3
summary(housing_price3)

#Calculate ANOVA
anova(housing_price, housing_price2, housing_price3)

#Predict our model(s):
predict(housing_price, data.frame(beds=10, baths, sqft, type), interval = 'confidence' )
