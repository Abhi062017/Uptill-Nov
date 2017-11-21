# Classificaton Decision trees (prediction by classification/decision tree algo)
# Algo: rpart
  data("iris")
  class(iris)
  
  head(iris) #note the species are alphabetically sorted
  summary(iris)
  
  #Step1) Shuffle the data
  set.seed(9850)
  random <- runif(nrow(iris)) #generates 150 numbers(that's nrow(iris)) betweeen 0 and 1
  iris2 <- iris[order(random),] #generates a new df with shuffled rows from iris dataset
  class(iris2)
  View(iris2) #compare with View(iris)
  
  #Step2) Invoking rpart
  library(rpart)
  model <- rpart(Species ~ .,   #notice, Species is our target,also denoted by 'y', and (~. means all the remaining variables, are our predictors).
        data = iris2[1:100,],      # this subsets the data in 2 parts (here data = train, the rest is test, which is 50 in this case)
        method = "class"          #note: if y is a factor than choose method = 'class'
        )
  model
    #plotting rpart
    install.packages('rpart.plot')
    library(rpart.plot)
    rpart.plot(model)
  
  #Step3) predicting using test(remainder of the data)
  p1 <- predict(model, iris2[101:150,], type = "class")
    #confusion matrix:
    p1
    length(p1)
    table(iris2[101:150,5],p1)
    