getwd()
#reading the datafile
#it's a '\t' separated file,and dec='.', hence use 'read.delim'
titanic <- read.delim('http://math.ucdenver.edu/RTutorial/titanic.txt', stringsAsFactors = F)
str(titanic)
summary(titanic) #Note the NA's

titanic <- titanic[,-1] #Name is not needed in prediction as it's unique
str(titanic)
#response variable should be a factor for it to be used in regression models
titanic$Survived <- as.factor(titanic$Survived)
str(titanic)

#should we be converting Sex to factor?
unique(titanic$Sex)

#checking for NA's
sum(is.na(titanic)) #557 total NA's
tail(titanic) #total records=1313, of which 557 have missing obs.
library(dplyr)
titanic %>% filter(!complete.cases(.)) #spits out the missing obs./incomplete cases
Age_NA <- titanic %>% filter(!complete.cases(.)) #forming a separate df of NA's
View(Age_NA)
summary(Age_NA)

#***********************************************************
#Task: Impute the missing NA's
#Approach 1:
library(VIM)
titanic_noNA <- kNN(titanic, k=37) #note: k is sqrt(n)
View(titanic_noNA)

#Further Analysis
names(titanic_noNA)
titanic_changed_Age <- titanic_noNA[,c(2,6)]
View(titanic_changed_Age)
titanic_imputedAges <- titanic_changed_Age %>% filter(Age_imp==TRUE)

#Approach 2:
mean(titanic$Age, na.rm = T)
summary(titanic)#fetch mean(Age)
titanic[is.na(titanic)] <- 30.40
View(titanic)

#Finalizing Approach:1, for time being, as our final df
names(titanic_noNA)
titanic_noNA <- titanic_noNA[,c(1:4)]
titanic_noNA %>% filter(!complete.cases(.)) #confirmation that the df has no NA's

#***************************************************************
#Since, our final df is set, we'd run the split-train-predict
#Step1: split
library(caret)
set.seed(97501)
index <- createDataPartition(y=titanic_noNA$Survived, p=0.75, list = F)
View(index)
train <- titanic_noNA[index,]
test <- titanic_noNA[-index, ]

#Step2.1: 10-fold cross validation, repeated 5 times
set.seed(97501)
cv.folds <- createMultiFolds(y=train$Survived, k=10, times=5)
cv.cntrl <- trainControl(method = 'repeatedcv',
                         number = 10,
                         repeats = 5,
                         index = cv.folds)
#Step2.2: train
library(doSNOW)
cl <- makeCluster(3, type = 'SOCK')
registerDoSNOW(cl)
rpart.cv.1 <- train(Survived ~ .,
                    data = train,
                    method = 'rpart',
                    trControl = cv.cntrl,
                    tuneLength = 7)
stopCluster(cl)
rpart.cv.1
nrow(train)

#Step3: prediction
pred <- predict(rpart.cv.1, newdata = test)
confusionMatrix(pred, test$Survived) #89.9% accuracy
