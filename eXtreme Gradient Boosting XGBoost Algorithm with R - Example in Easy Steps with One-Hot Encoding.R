#eXtreme Gradient Boosting - One-Hot Encoding
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/2.codes')
sessionInfo()

#invoking required packages
library(dplyr)
library(Matrix)
install.packages("xgboost") #note xgboost only works with numeric vectors
install.packages('magrittr')
library(xgboost)
library(magrittr)

#read file
data <- read.csv(file.choose(), header = T)
head(data)
unique(data$rank)
glimpse(data)

#casting a numeric variable to factor
data$rank <- as.factor(data$rank)
glimpse(data)

#Partitioning data:
set.seed(1234)
s <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[s==1,]
test <- data[s==2,]


#Create a matrix : One-Hot encoding for factor variables

  #One hot encoding : is a process by which categorical variables,
  #are converted into a form(numeric vector form) that could be provided,
  #to ML algorithms to do a better job in prediction.

  train_sparse_matrix <- sparse.model.matrix(admit ~. -1, data = train)
  head(train_matrix)
  train_label <- train[, 'admit']
  #creating Dense matrix using sparse matrix
  train_dense_matrix <- xgb.DMatrix(label = train_label,
                              data = as.matrix(train_sparse_matrix))
  
  #performing the same steps for test
  test_sparse_matrix <- sparse.model.matrix(admit ~ . -1, data = test)
  test_label <- test[, 'admit']
  test_dense_matrix <- xgb.DMatrix(label = test_label,
                             data = as.matrix(test_sparse_matrix))
  

#Parameters
nc <- length(unique(train_label))
xgb.param <- list('objective' = 'multi:softprob',
             'eval_metric' = 'mlogloss',
             'num_class' = nc)
watchlist <- list(train = train_dense_matrix,
                  test = test_dense_matrix)


#Extreme Gradient Boosting Model
model <- xgb.train(params = xgb.param,
                   data = train_dense_matrix,
                   nrounds = 100,
                   watchlist = watchlist
                   )
model #note the attribute called 'evaluation_log'


#Train and Test error plot
model$evaluation_log
e <- as.data.frame(model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue') #error dips as iteration increases.
lines(e$iter, e$test_mlogloss, col = 'magenta') #it looms large, after a small dip

  #check out the lowest dip point of error
  min(e$train_mlogloss) #0.082495 is the least error point for train dataset
  min(e$test_mlogloss) #0.595096 is the least error point for test dataset
  e[e$train_mlogloss == 0.082495,] #iteration no. 100 produced the least error point
  e[e$test_mlogloss == 0.595096,]  #iteration no. 3 produced the least error point
  
    #2nd attempt : tuning xgboost by changing 'eta' and 'nrounds'
    model <- xgb.train(params = xgb.param,
                       data = train_dense_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       eta = 0.05) #note: 'eta' ranges from 0 to 1
      #the lower the 'eta' the lesser the over-fitting, by default it's 0.3
      e <- as.data.frame(model$evaluation_log)
      plot(e$iter, e$train_mlogloss, col = 'blue')
      lines(e$iter, e$test_mlogloss, col = 'magenta')
      min(e$train_mlogloss) #notice, the error point has increased, hence less over-fit
      min(e$test_mlogloss)
      e[e$test_mlogloss == 0.597642,] #iter no. 35 produces the least error point

      #3rd attempt:
      model <- xgb.train(params = xgb.param,
                         data = train_dense_matrix,
                         nrounds = 35,
                         watchlist = watchlist,
                         eta = 0.05)
      e <- as.data.frame(model$evaluation_log)
      plot(e$iter, e$train_mlogloss, col = 'blue')
      lines(e$iter, e$test_mlogloss, col = 'magenta')
      min(e$train_mlogloss)#error point has increased further, even lesser over-fit 
      min(e$test_mlogloss)#error point has increased further, even lesser over-fit 
      
      #Deduction: we get the best fit when 'nrounds' is 35, and 'eta' is 0.05


#Feature Importance:
imp <- xgb.importance(colnames(train_dense_matrix), model = model)
print(imp) #notice 'Gain' displays the importance of variables
xgb.plot.importance(imp) #this showcases the important variables, gpa standsout.


#Prediction and Confusion Matrix - test data.
#To create confusion matrix, firstly arrange the predictions in df format
p <- predict(model, newdata = test_dense_matrix)
pred <- matrix(data = p, nrow = nc, ncol = length(p)/nc) %>% 
  t() %>% #transpose the matrix
  data.frame() %>% #change from matrix to dataframe
  mutate(label = test_label, max_prob = max.col(., 'last')-1)

head(pred) #notice misclassification for row no. 5

#Confusion Matrix:
table(Predictions = pred$max_prob, Actual = pred$label)
cf <- table(Predictions = pred$max_prob, Actual = pred$label)
acc <- sum(diag(cf))/sum(cf) #66.6 % accuracy


#****************************************************************************
#Further enhancing/tuning the 'xgboost parameters', for better accuracy
#****************************************************************************
model <- xgb.train(params = xgb.param,
                   data = train_dense_matrix,
                   nrounds = 200,
                   watchlist = watchlist,
                   eta = 0.01,
                   max.depth = 3,
                   gamma = 0,
                   subsample = 1,
                   colsample_bytree = 1,
                   missing = NA,
                   seed = 333
                   )
e <- data.frame(model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue') #notice, overfit has reduced even further
lines(e$iter, e$test_mlogloss, col = 'magenta')#notice, overfit has reduced even further
min(e$train_mlogloss)
min(e$test_mlogloss)
e[e$test_mlogloss == 0.625217,] #iter no. 104 spits out the least error point

  #Hence, changing the value of 'nrounds' to 104 :
  model <- xgb.train(params = xgb.param,
                     data = train_dense_matrix,
                     nrounds = 104,
                     watchlist = watchlist,
                     eta = 0.01,
                     max.depth = 3,
                     gamma = 0,
                     subsample = 1,
                     colsample_bytree = 1,
                     missing = NA,
                     seed = 333
  )
  e <- data.frame(model$evaluation_log)
  plot(e$iter, e$train_mlogloss, col = 'blue') #notice, overfit has reduced even further
  lines(e$iter, e$test_mlogloss, col = 'magenta')#notice, overfit has reduced even further
  min(e$train_mlogloss)
  min(e$test_mlogloss)
  
  p <- predict(model, newdata = test_dense_matrix)
  pred <- matrix(data = p, nrow = nc, ncol = length(p)/nc) %>% 
    t() %>% #transpose the matrix
    data.frame() %>% #change from matrix to dataframe
    mutate(label = test_label, max_prob = max.col(., 'last')-1)
  
  head(pred) #notice misclassification for row no. 5
  
  #Confusion Matrix:
  table(Predictions = pred$max_prob, Actual = pred$label)
  cf <- table(Predictions = pred$max_prob, Actual = pred$label)
  acc <- sum(diag(cf))/sum(cf) #70.6 % accuracy
  