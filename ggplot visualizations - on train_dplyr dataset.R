getwd()
#read the dataset
titanic <- read.csv('train.csv', stringsAsFactors = FALSE)
str(titanic)

#Set factors
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)

sum(is.na(titanic$Title)) #642, 711 are the PassengerId that have no Title
#Imputing Title to missing records
titanic[which(titanic$PassengerId == 642), 'Title'] <- 'Mrs.'
titanic[which(titanic$PassengerId == 711), 'Title'] <- 'Mrs.'

library(ggplot2)
#Question.1: How many Survived and how many didn't?
ggplot(titanic, aes(x = Survived)) +
    geom_bar()

#Question.2: What was the Age distribution on the ship?
ggplot(titanic, aes(x = Age)) +
    geom_histogram(binwidth = 5)

#Question.3: What was the Title distribution on the ship?
ggplot(titanic, aes(x = Title)) +
    geom_bar()

#Question.4: What was the Fare distribution among the passengers?
ggplot(titanic, aes(x = Fare)) +
    geom_histogram(binwidth = 5)

#Question.5: What was the 'Passengers travelling with children' distribution?
ggplot(titanic, aes(x = child.count)) +
    geom_histogram(binwidth = 0.5)

#Question.6: What was the Survival rate across the entire Age spread of the passengers?
ggplot(titanic, aes(x = Survived, y = Age)) +
    geom_boxplot()

#Question.7: What was the Survival rate across the entire Age spread of the passengers?
ggplot(titanic, aes(x = Age, fill = Survived)) +
    geom_histogram(binwidth = 2)

#Question.8: What was the Survival rate with respect to the Pclass?
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
    geom_bar()

#Question.9: What was the Survival rate across the Pclass with respect to the Title?
ggplot(titanic, aes(x = Title, fill = Survived)) +
    facet_wrap(~ Pclass) +
    geom_bar()

#Question.10: What was the Survival rate of all the Titles across Pclass?
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
    facet_wrap(~ Title) +
    geom_bar()

#Question.11: What was the Survival rate by gender across Pclass?
ggplot(titanic, aes(x = Sex, fill = Survived)) +
    facet_wrap(~ Pclass) +
    geom_bar()

#Question.12: What was the Survival rate by Pclass across Gender?
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
    facet_wrap(~ Sex) +
    geom_bar()

#Question.13: What was the Survival rate by the Embarked points?
ggplot(titanic, aes(x = Embarked, fill = Survived)) +
    geom_bar()

#Question.14: What was the Survival rate by the Embarked points across the Pclass?
ggplot(titanic, aes(x = Embarked, fill = Survived)) +
    facet_wrap(~ Pclass) +
    geom_bar()

#Question.15: What was the Survival rate by the Embarked points across the Gender?
ggplot(titanic, aes(x = Embarked, fill = Survived)) +
    facet_wrap(~ Sex) +
    geom_bar()

#Question.16: What was the Survival rate by the Embarked points across the Pclass and Gender?
ggplot(titanic, aes(x = Embarked, fill = Survived)) +
    facet_wrap(~ Pclass + Sex) +
    geom_bar()

#Question.17: What was the Survival rate by the Embarked points across the Gender and Pclass?
ggplot(titanic, aes(x = Embarked, fill = Survived)) +
    facet_wrap(~ Sex + Pclass) +
    geom_bar()

#Question.18: What was the density of the Survival rate by Age?
ggplot(titanic, aes(x = Age, fill = Survived)) +
    geom_density()

#Question.19: What was the density of the Survival rate by Age across Pclass?
ggplot(titanic, aes(x = Age, fill = Survived)) +
    facet_wrap(~Pclass) +
    geom_density()

#Question.20: What was the density of the Survival rate by Age across Gender?
ggplot(titanic, aes(x = Age, fill = Survived)) +
    facet_wrap(~Sex) +
    geom_density()

#Question.21: What was the density of the Survival rate by Age across Sex and Pclass?
ggplot(titanic, aes(x = Age, fill = Survived)) +
    facet_wrap(~Sex + Pclass) +
    geom_density()
