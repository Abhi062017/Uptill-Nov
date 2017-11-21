#Decision Tree on iris dataset
getwd()
data("iris")

#Split
s <- sample(2, nrow(iris), replace = T, prob = c(0.7,0.3))#note the probs will be approx
train <- iris[s==1,]
test <- iris[s==2,]

  #verification
  library(dplyr)
  a<- as.data.frame(s)
  a%>%count(a==1)

#Invoke the decision tree
set.seed(9850)
install.packages('party')
library(party)
formula <- Species ~ .
model <- ctree(formula = formula, data = train)
print(model)
plot(model, type = 'simple')

p1 <- predict(model, test)
table(test[,5], p1) #prediction based on testing data
mean(test[,5] == p1) #93.6% accuracy


#Outcome of the predicted results Vs. the actual data
outcome <- table(predict(model), train[,5]) #prediction based on training data
accuracy <- sum(diag(outcome))/(3+sum(diag(outcome)))
accuracy #97.08% accuracy

table(predict(model))
length(predict(model))
class(predict(model))


