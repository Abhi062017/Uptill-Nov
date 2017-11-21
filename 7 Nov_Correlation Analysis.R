#Correlation Analysis
getwd()
#read dataset
data <- read.csv(choose.files(), header = T)
View(data)

#running some descriptive stats
str(data) #14 exploratory variables + 1 response variable (medv)
summary(data)
sum(is.na(data))#no NA's

#method1
install.packages("psych")
library(psych)
pairs.panels(data)
temp <- cor(data)
class(temp)
View(temp)
temp[!lower.tri(temp)] <- 0 #since diag and upper triangle values are redundant
data.new <- data[, apply(temp,2, function(x) any(x>0.5))]#oppsoite of any(abs(x)>0.5)
class(data.new)
View(data.new)
pairs.panels(data.new)

#method2
install.packages("caret")
library(caret)
temp2 <- cor(data)
fc <- findCorrelation(temp2, cutoff = 0.5)
class(fc)
fc
fc <- sort(fc)
fc
data_reduced <- data[, c(fc)]
View(data_reduced)
names(data.new)
names(data_reduced)
pairs.panels(data_reduced)

#plotting
plot(data_reduced$rad, data_reduced$tax) #best relation
cor(data_reduced$rad, data_reduced$tax)
plot(data_reduced$medv, data_reduced$dis)
cor(data_reduced$medv, data_reduced$dis)
