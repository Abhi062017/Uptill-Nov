# First Kaggle Submission
# Scope Of Improvement: Just like Fare, we can finetune Age, and other such variables.
getwd()
setwd("C:/Users/abhi/Downloads/")

titanic.train <- read.csv(file = "train.csv",
                          stringsAsFactors = FALSE,
                          header = TRUE
                          )

titanic.test <- read.csv(file = "test.csv",
                          stringsAsFactors = FALSE,
                          header = TRUE
                         )


median(titanic.train$Age, na.rm = TRUE)
median(titanic.test$Age, na.rm = TRUE)


titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

ncol(titanic.train)
ncol(titanic.test)
nrow(titanic.train)
nrow(titanic.test)

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)
ncol(titanic.full)
nrow(titanic.full)

table(titanic.full$IsTrainSet)
table(titanic.full$Embarked) #Missing values

length(which(titanic.full$Embarked == ""))
titanic.full[titanic.full$Embarked == "", 'Embarked'] <- 'S' # NAs removed
table(titanic.full$Embarked)

table(is.na(titanic.full$Age)) # Lots of NAs
age.median <- median(titanic.full$Age, na.rm = TRUE) #calculating median of Ages
age.median
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median #Inserts median of Age for NAs
table(is.na(titanic.full$Age)) # Confirmation

table(is.na(titanic.full$Fare)) # 1 NA for Fare
# We'll use linear regression model to populate NA for Fare
  # fare.median <- median(titanic.full$Fare, na.rm = TRUE) #calculating median of Fare
  # fare.median
  # titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median #Inserts median of Fare for NAs
  # table(is.na(titanic.full$Fare)) # Confirmation
boxplot(titanic.full$Fare) # outliers start from somewhere around 77
# Lets confirm that
boxplot.stats(titanic.full$Fare) # will give all the values of Fare
boxplot.stats(titanic.full$Fare)$stats # this gives the values by min/1st quartile/half/3rd quartile/max
boxplot.stats(titanic.full$Fare)$stats[5] # gives the max value (upper bound)
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5] # setting the Fare limit
outlier.filter <- titanic.full$Fare < upper.whisker # place holder for all fares < upper bound
titanic.full[outlier.filter, ] #returns all the rows and columns with fare < upper bound

fare.equation <- "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter, ]
)

fare.row <- titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass" , "Sex" , "Age" , "SibSp" , "Parch", "Embarked")
  ]

fare.predictions <- predict(fare.model,newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
titanic.full[1044,]

# Categorical Casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

str(titanic.full)

# Now overwriting the 'test' and 'train' datasets with these changes
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE, ]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE, ]

str(titanic.train)
str(titanic.test)

titanic.train$Survived <- as.factor(titanic.train$Survived)


# Predicting the Model
library(randomForest)

# equating the prediction equation (y ~ a+b+c...)
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

titanic.model <- randomForest(formula = survived.formula,
             data = titanic.train,
             ntree = 500,
             mtry = 3,
             nodesize = 0.01 * nrow(titanic.test))
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

# Our Kaggle sunmission file
write.csv(output.df, file = 'Kaggle_Submission#1_Titanic_7Oct17.csv', row.names = FALSE)