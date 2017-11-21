#*******************************************************************************
# prediction on iris dataset using 'rpart'
#*******************************************************************************
data("iris")
class(iris)

head(iris) #note the species are alphabetically sorted
summary(iris)

#Step1) Shuffle the data
set.seed(9850)
random <- runif(nrow(iris)) 
iris2 <- iris[order(random),]
class(iris2)
View(iris2)

#Step2) Invoking rpart
library(rpart)
model <- rpart(Species ~ ., iris2[1:100,], method = "class")
model

#plotting rpart
library(rpart.plot)
rpart.plot(model)

#Step3) predicting using test(remainder of the data)
p1 <- predict(model, iris2[101:150,], type = "class")
#confusion matrix:
p1
length(p1)
table(iris2[101:150,5],p1)


#*******************************************************************************
# prediction on iris dataset using 'knn'
#*******************************************************************************
data("iris")
summary(iris)

#Step 1: Randomize the data
set.seed(9850)
runif(22) #spits out 22 different values between 0 and 1
random <- runif(nrow(iris))
random
summary(random)

iris2 <- iris[order(random),] #randomizes the values in iris dataset
head(iris2)
View(iris2)

#Step 2: Normalize the data
normalize <- function(x) {
  return((x-min(x))/(max(x) - min(x)))} #standard formula for Normalization

iris_normalized <- as.data.frame(lapply(iris2[,-5], normalize))

#Split data into test/train:
iris_train <- iris_normalized[1:129,]
iris_test <- iris_normalized[130:150,]

iris_train_target <- iris2[1:129, 5] #this will form 'cl' for knn
iris_test_target <- iris2[130:150, 5] #this will be used to compare prediction

#invoke required package
library(class) #knn algo is a 'class' package algo
#calculating k
sqrt(150) # sqrt(150) = 12.24, and we should always take the odd value, so k = 13
model <- knn(train = iris_train, test = iris_test, cl = iris_train_target, k = 13)
model

#Confusion matrix:
table(iris_test_target, model)
