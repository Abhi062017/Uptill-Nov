#Random Forest on iris dataset
getwd()
sessionInfo()

data("iris")
summary(iris)
nrow(iris)

#split
set.seed(9850)
s <- sample(nrow(iris), 100)
length(s)
class(s)
s

train <- iris[s,]
test <- iris[-s,]

#invoke randomforest
library(randomForest)
model <- randomForest(Species ~., data = train, importance = TRUE)
print(model)

p1 <- predict(model, test)
table(test[,5], p1)
mean(test[,5]==p1) #this spits out the accuracy of our model
importance(model)
getTree(model, 500, labelVar = T) #spits out details of each of the tree used in the rf
