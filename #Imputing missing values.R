#Imputing missing values
getwd()
data(iris)

install.packages("missForest") #to use 'prodNA' function that generates random NA's
library(missForest)

iris.miss <- prodNA(iris, noNA = 0.1) #noNA = percentage of NA's to be produced
tbl_df(iris.miss)

sum(is.na(iris.miss)) #NA's in the 'iris.miss' dataset
Total.NAs <- sum(is.na(iris.miss$Petal.Length)) +
  sum(is.na(iris.miss$Petal.Width)) +
  sum(is.na(iris.miss$Sepal.Length)) +
  sum(is.na(iris.miss$Petal.Width)) +
  sum(is.na(iris.miss$Species))
Total.NAs #note sum of individual variable's NA's may/maynot match with total NA's



iris.subset <- subset(iris, select = -5)
iris.subset



install.packages("mice")
library(mice)
mice::md.pattern(iris.subset)
md.pattern(iris.subset)
