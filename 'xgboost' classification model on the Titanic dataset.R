#Performing 'xgboost' classification model on the Titanic dataset
#Note: Always follow the approach of "train-predict-train-predict...' , for better results.
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/')

#Pre-requisite packages needed in this attempt:
install.packages('xgboost')
install.packages('devtools')
devtools::install_github('topepo/caret/pkg/caret')
install.packages('doSNOW')

library(caret)
library(devtools)
library(doSNOW)
library(xgboost)

#Read data
train <- read.csv('train.csv', stringsAsFactors = FALSE)
View(train)

#Data Wrangling
  #Replace missing 'embarked' values with mode value
  table(train$Embarked)
  train$Embarked[train$Embarked == ""] <- 'S'
  
  #Add a feature 'Missing Ages', to track missing ages in the dataset
  summary(train$Age)
  train$MissingAge <- ifelse(is.na(train$Age), 'Y','N')
  
  #Add a feature for family size
  train$FamilySize <- 1 + train$SibSp + train$Parch
  
  #Setup factors
  str(train)
  train$Survived <- as.factor(train$Survived)
  train$Pclass <- as.factor(train$Pclass)
  train$Sex <- as.factor(train$Sex)
  train$Embarked <- as.factor(train$Embarked)
  train$MissingAge <- as.factor(train$MissingAge)
  
  #Subset data to features we wish to keep/use
  str(train)
  train <- train[, c(2,3,5,6,7,8,10,12,13,14)]
  
  #***********************************************
  #Impute Missing Ages
  #***********************************************
    #caret supports a number of mechanisms of imputing/predicting
    #missing values. Leverage 'bagged decision trees',
    #to impute missing values for the Age feature.
  
    #First, transform all features to dummy variables
    dummy.vars <- dummyVars(~ ., data = train[,-1]) #train
    train.dummy <- predict(dummy.vars, train[,-1])  #predict
    View(train.dummy)
    class(dummy.vars)
    class(train.dummy)
    
    #Now, impute!
    pre.process <- preProcess(train.dummy, method = 'bagImpute') #train
    imputed.data <- predict(pre.process, train.dummy)            #predict
    View(imputed.data)
    class(pre.process)
    class(imputed.data)
    
    train$Age <- imputed.data[,6]
    View(train)
    table(is.na(train$Age)) #verification
    
#Split the dataset
  #Use caret to create a 70%/30% stratified split of the training data,
  #keeping the proportions of survived class label the same across the splits.
  set.seed(54321)
  index <- createDataPartition(train$Survived,
                               times = 1,
                               p = 0.7,
                               list = FALSE)
  
  titanic.train <- train[index,]
  titanic.test <- train[-index,]
  
  #Examine the proportions of the survived class label across the datasets.
  prop.table(table(train$Survived))
  prop.table(table(titanic.train$Survived))
  prop.table(table(titanic.test$Survived))
  

#*****************************************************************************
# Train Model
#*****************************************************************************
  #Set up caret to perform 10-fold cross validation repeated 3 times,
  #and to use a grid search for optimal hyper-parameter values
  train.control <- trainControl(method = 'repeatedcv',
                                number = 10,
                                repeats = 3,
                                search = 'grid')
  
  #Leverage a grid search of hyperparameters for xgboost.
  tune.grid <- expand.grid(eta = c(0.5, 0.075, 0.1),
                           nrounds = c(50,75,100),
                           max_depth = 6:8,
                           min_child_weight = c(2.0,2.25,2.5),
                           colsample_bytree = c(0.3,0.4,0.5),
                           gamma = 0,
                           subsample = 1)
  View(tune.grid)
  
  #Use doSNOW package to enable caret to train in parallel.
  #Create a socket cluster using 3 processes
  cl <- makeCluster(3, type = 'SOCK')
  #Register cluster so that caret will know to train in parallel
  registerDoSNOW(cl)
  
  #Train the xgboost model using 10-fold CV repeated 3 times
  #and a hyperparameter grid search to train the optimal model
  caret.cv <- train(Survived ~ .,
                    data = titanic.train,
                    method = 'xgbTree',
                    tuneGrid = tune.grid,
                    trControl = train.control)
  stopCluster(cl)
  
  #Examine caret's processing results
  caret.cv
  
  #Make predictions on the test set using a xgboost model
  #trained on all 625 rows of the training set using the found
  #optimal hyper-parameter values
  preds <- predict(caret.cv, titanic.test)
  
  #Use caret's confusionMatrix() function to estimate the
  #effectiveness of this model on unseen, new data.
  confusionMatrix(preds, titanic.test$Survived)
  xgb.importance(feature_names = colnames(titanic.train),
                 model = caret.cv$finalModel)