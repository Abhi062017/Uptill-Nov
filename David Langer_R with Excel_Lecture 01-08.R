getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R')

# Lecture 03 : R Programming for Excel Users using titanic dataframe
titanic_train <- read.csv('titanic_train.csv')   #reading the dataset
str(titanic_train)
library(dplyr)
titanic_train %>% head()
glimpse(titanic_train)

# Subset the data for the Age variable (i.e 6th column)
Age_subset <- titanic_train[, 6]
glimpse(Age_subset)
remove(Age_subset)    #dropping the place holder

# Sum of all Age values
Age_sum <- sum(Age_subset, na.rm = TRUE)
Age_sum
remove(Age_sum)
  # Alternatively:
  sum(titanic_train[,6], na.rm = TRUE)

# Subset the data for Age, Sibsp, Parch variables
str(titanic_train)
Age_Sibsp_Parch_Subset <- titanic_train[,6:8]
head(Age_Sibsp_Parch_Subset)
tail(Age_Sibsp_Parch_Subset)
remove(Age_Sibsp_Parch_Subset)

# Perform vector summation for first 10 Age values
vct_sum_Age_10 <- sum(Age_subset[1:10], na.rm = TRUE)
vct_sum_Age_10
Age_subset[1:10]
  # Alterntaively
  sum(titanic_train[1:10,6], na.rm = TRUE)

# 3 ways to add a new column as size of the family
titanic_train$family_size <- NA
titanic_train$family_size <- ''
titanic_train$family_size <- NaN
str(titanic_train)
tail(titanic_train)

# Add the column for the family size, as a vectorized calculation
titanic_train$family_size <- 1 + titanic_train$SibSp + titanic_train$Parch
str(titanic_train)
tail(titanic_train)
View(titanic_train)

# Add an empty column 'AgeMissing' to the dataframe
titanic_train$AgeMissing <- ''
View(titanic_train)
  # Now, populate the new column using an ifelse
  titanic_train$AgeMissing <- ifelse(is.na(titanic_train$Age), 'Y','N')
  str(titanic_train)
  View(titanic_train)

# Lecture 04: Investigate the nature of Age column : summary/sum/length/median/variance/sd
summary(titanic_train$Age)
sum(titanic_train$Age, na.rm = TRUE)
length(titanic_train$Age)
median(titanic_train$Age, na.rm = TRUE)
var(titanic_train$Age, na.rm = TRUE)
sd(titanic_train$Age, na.rm = TRUE)   # sd = sqrt(var) : sqrt(211.0191)

"""Since the values are missing in the Age column, create a variable/place holder 
which stores just the non-missing Age values"""

Age_non_missing <- titanic_train$Age[!is.na(titanic_train$Age)]
length(Age_non_missing)

# Lecture 05: Slice Age data by gender
str(titanic_train)
female_Age <- titanic_train$Age[titanic_train$Sex == 'female']
length(female_Age)
summary(female_Age)

male_Age <- titanic_train$Age[titanic_train$Sex != 'female']
length(male_Age)
summary(male_Age)

female_Age
class(female_Age)
head(female_Age)
View(titanic_train)

  # Investigate the new vectors (female_Age and male_Age)
  summary(female_Age)
  sd(female_Age, na.rm = TRUE)
  
  summary(male_Age)
  sd(male_Age, na.rm = TRUE)
  
# (*)Slice and create a new set of datasets, by Sex
female_train <- titanic_train[titanic_train$Sex == 'female', ]
View(female_train)

male_train <- titanic_train[titanic_train$Sex == 'male', ]
View(male_train)

  #Further slice the dataframe, by Pclass of 1
  female_1st_train <- female_train[female_train$Pclass == 1,]
  View(female_1st_train)
  summary(female_1st_train$Age)  #Investigate the Age variable of the new dataframe
  
  male_1st_train <- male_train[male_train$Pclass ==1, ]
  View(male_1st_train)
  summary(male_1st_train$Age)    #Investigate the Age variable of the new dataframe
  
# Reducing the above (*) code in one expression:
female_1st_train <- titanic_train[titanic_train$Sex == 'female' & titanic_train$Pclass == 1, ]
length(female_1st_train)  #it spits out the total no of variables/columns
View(female_1st_train)

male_1st_train <- titanic_train[titanic_train$Sex == 'male' & titanic_train$Pclass ==1, ]
length(male_1st_train)
View(male_1st_train)


# Lecture 06: Slice the dataset and create a subset based on columns of choice
names(titanic_train)  #this spits out variable names of the dataset
my_subset1 <- titanic_train[, c(2,3,5,6:8,10,13,14)]
View(my_subset1)

# Filtering rows and columns by positions
my_row <- c(1:10, 67,69,78:105)
my_col <- c(2:5,8,9)
my_subset2 <- titanic_train[my_row, my_col]
View(my_subset2)
  #Filtering only the rows which are required, by discarding the ones that are not
  my_subset3 <- titanic_train[-c(10:884,886, 889:891), -c(1:5,7,10:12)]
  View(my_subset3)

# Lecture 07: Invoke ggplot
library(ggplot2)
ggplot(female_1st_train, aes(x=Age)) + 
  geom_histogram(binwidth = 2)               #each bin is of width, Age=2

# Lecture 08: Enhance ggplot
ggplot(female_1st_train, aes(x=Age)) + 
  geom_histogram(binwidth = 2) +
  theme_classic() + 
  labs(x = 'Pasenger Age',
       y = 'Passenger Count',
       title = 'Female 1st Class Age Distribution')

# Invoke facet_wrap
ggplot(female_train, aes(x=Age)) +
  geom_histogram(binwidth = 5) +   #binwidth changed to size of each bin of Age=5
  facet_wrap(~ Pclass) +           #similar to Excel's pivot
  theme_classic() +
  labs(x = 'Passenger Age',
       y = 'Passenger Count',
       title = "All aboard Female's Age Distribution by Pclass")

#Invoking facet_wrap on Embarked variable
ggplot(female_train, aes(x=Age)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ Embarked) +          #similar to Excel's pivot
  theme_classic() +
  labs(x = 'Passenger Age',
       y = 'Passenger Count',
       title = "All aboard Female's Age Distribution by Embarkment points")


# Automatically pivot by Pclass and Survived!
str(female_train) #note: Survived variable isn't a factor, which it should be.
female_train$Survived = as.factor(female_train$Survived) #converting the dtype from int to factor

ggplot(female_train, aes(x=Age, fill = Survived)) +  #'fill' values has to be a factor
  geom_histogram(binwidth = 5) +
  theme_classic() +
  facet_wrap(~ Pclass) +
  labs(x = 'Passenger Age',
       y = 'Passenger Count',
       title = 'Female survival rate by Pclass and Age')

  
  
# ggplot to display the Survival rates by Sex, Pclass and Age
str(titanic_train) #note: Survived isn't a factor
titanic_train$Survived <- as.factor(titanic_train$Survived)

ggplot(titanic_train, aes(x=Age, fill=Survived)) +
  geom_histogram(binwidth=5) +
  theme_classic() +
  facet_wrap(Sex ~ Pclass) +  #note: it'll plot all the Pclass for one Sex at a time
  labs(x = 'Passenger Age',
       y = 'Passenger Count',
       title = 'Survival rate of passengers by Sex, Pclass and Age')
  
  #plotting the above plot by inter-changing the facet_wrap values
  ggplot(titanic_train, aes(x=Age, fill=Survived)) +
    geom_histogram(binwidth=5) +
    theme_classic() +
    facet_wrap(Pclass ~ Sex) +  #note: it'll plot all Sex for one Pclass at a time.
    labs(x = 'Passenger Age',
         y = 'Passenger Count',
         title = 'Survival rate of passengers by Sex, Pclass and Age')
