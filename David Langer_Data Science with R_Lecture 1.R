getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R')

# Loading datasets
test <- read.csv('test.csv', header = TRUE)
train <- read.csv('train.csv', header = TRUE)

str(test)
str(train)

# Removing a variable ('PassengerId') from both the dataframes
test <- test[,-c(1)]
train <- train[,-c(1)]

View(test)
View(train)

library(stringr)  #loading the 'stringr' package
help(package = 'stringr')  #accessing 'help' on the package

# Add a 'Survived' variable to 'test', to allow for combining datasets
test.survived <- data.frame(Survived = rep('None', nrow(test)), test[,])
View(test.survived)

# Combine the datasets, as they are at par with each other after equalling the no.of variables
data.combined <- rbind(train, test.survived)

str(data.combined) #note: Survived/Pclass should be factor, not char/int.
  # changing the datatypes to factor
  data.combined$Survived <- as.factor(data.combined$Survived)
  data.combined$Pclass <- as.factor(data.combined$Pclass)
  
# A look at gross survival rates
table(data.combined$Survived)
table(data.combined$Pclass)

library(ggplot2)
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.8) + #note: here couldn't use 'geom_histogram'.
  xlab('Pclass') +
  ylab('Total Count') +
  labs(fill = 'Survived')

head(as.character(train$Name)) # as.character(), is used as 'Name' is a factor not a character.
length(unique(as.character(data.combined$Name))) #note: 2 names are duplicate.

# Get the duplicate names, and store them as a vector.
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), 'Name'])
dup.names

# check if the duplicated names are in data.combined or not
data.combined[which(data.combined$Name %in% dup.names),] #we infer that they are 4 different people, based on their age/ticket


# Working on 'Mr.' and 'Miss.'.
# Load stringr package
library(stringr)

# Extracting the 'Miss.' from the dataset
misses <- data.combined[which(str_detect(data.combined$Name, 'Miss.')),]
misses[1:5,] #note: a look at 'SibSp' and 'Parch' tells you that 'Miss.' were the unmarried girls.
class(misses)
str(misses)

# Extracting the 'Mrs.' from the dataset
mrses <- data.combined[which(str_detect(data.combined$Name, 'Mrs.')),]
mrses[1:5,]

# Extracting males from the dataset
males <- data.combined[which(data.combined$Sex =='male'),]
males[1:5,]

# Creating a utility function to help with the 'title' extraction from the dataframe
extractTitle <- function(Name) {
  Name <- as.character(Name)
  if(length(grep('Miss.', Name))>0) {
    return('Miss.')
  }else if(length(grep('Mrs.', Name))>0) {
    return('Mrs.')
  }else if(length(grep('Master.', Name))>0) {
    return('Master')
  }else if(length(grep('Mr.', Name))>0) {
    return('Mr.')
  }else
    return('Other')
}


titles <- NULL #place holder for all the extracted titles
for(i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i,'Name']))
}

data.combined$title <- as.factor(titles)  #converting the titles' datatype to it's appropriate dtype : 'factor', and cramming it in the dataframe with a new variable
View(data.combined)


# Since we only have the Survived variable for the train dataset and not for the data.combined yet
# so, we'll use only the first 891 rows of the data.survived dataset (which is equal to train dataset)

ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  geom_bar(width = 0.8) +
  theme_classic() +
  facet_wrap(~Pclass) +
  ggtitle('Survival rate of passengers by Pclass') +
  xlab('Title') +
  ylab('Total Count') +
  labs(fill = 'Survived')


# Distribution of males to females across train
table(data.combined$Sex)
