#Split data 80/20% and then predict
library(caret)
library(lubridate)
data(hflights)
names(hflights)

hf <- hflights[1:100,-c(21,18,20,19,1,2,3,4,14,15,17,18)]
View(hf)
names(hf)

#Split:
ind <- sample(2, size = nrow(hf), replace = TRUE, prob = c(0.7,0.3))
train_hf <- hf[ind==1,]
test_hf <- hf[ind==2,]

head(train_hf)
head(test_hf)


#Multi linear regression model:
mlr_train_hf <- lm(DepDelay ~ DepTime+ArrTime+ActualElapsedTime+AirTime+ArrDelay+Distance, train_hf)
summary(mlr_train_hf)

#Predict:
pred <- predict(mlr_train_hf, test_hf)
head(pred)
head(test_hf) #compare the head(pred) with head(test_hf) to see how close were the predictions

#Stratified split:
library(caret)
ind2 <- createDataPartition(hf$DepDelay, times = 1, p = 0.7, list = FALSE)
train <- hf[ind2,]
test <- hf[-ind2,]

prop.table(table(hf$DepDelay))
prop.table(table(train$DepDelay))
prop.table(table(test$DepDelay))

#multilinear regression model of the stratified split:
mlr2 <- lm(DepDelay ~ DepTime+ArrTime+ActualElapsedTime+AirTime+ArrDelay+Distance, train)
summary(mlr2)

#Predict:
pred2 <- predict(mlr2, test)
head(pred2)
head(test)
head(pred)

library(tm)