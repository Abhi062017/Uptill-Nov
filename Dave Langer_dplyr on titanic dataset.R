#Dave Langer's_6 video long 'Data Wrangling & Feature Engg with dplyr, series'
#********************************************************************
#Lecture#1
getwd()

#Load the libraries
library(dplyr)
library(stringr)

help(package = 'dplyr')
help(package = 'stringr')

#Load data
train <- read.csv('titanic_train.csv', stringsAsFactors = FALSE)
glimpse(train)
View(train)

#*******************************************************************
#Lectur#2
#What about the zero fares in our dataset?
zero.fare <- train %>% filter(Fare == 0) #15 obs with zero fare
#Note: we could have alternatively used the below 3:
#1). nrow(filter(train, Fare == 0))
#2). nrow(train %>% filter(Fare == 0))
#3). nrow(train[which(train$Fare == 0),])

View(zero.fare)
#Deduction from zero.fare:
#1). All passengers with zero fare are 'male'.
#2). Maybe all of them are men(and not boys).
#3). All of them embarked from 'Southampton'.
#4). Just one of the 15 survived.
#5). Traveling alone (No values for 'Parch' & 'SipsSp').

#Let's consider zero.fare out new subset of data,
#and just work on it further by collecting Pclass values for it.
zero.fare.pclass <- zero.fare %>%
    group_by(Pclass)%>% #this'll pivot by Pclass
    summarize(Total = n())%>% #create a new place holder 'Total' which stores total no.of rows returned by 'group_by'
    arrange(desc(Total)) #sorts the place holder in descending order

View(zero.fare.pclass) #Deduction: Nothing significant
class(zero.fare) #dataframe
class(zero.fare.pclass) #tibble (a trimmed down version of dataframe)

#Also Note: dplyr's group_by() needs summarize to run along,
#hence the below code won't return the desired result,:
zero.fare.new <- zero.fare %>% group_by(Pclass)
#unless followed by summarize(), like in the below code:
zero.fare.new <- zero.fare %>% group_by(Pclass) %>% summarize(Total = n())

#*******************************************************************
#Lectur#3
#Add the new feature for the Title of each passenger
#as this will work as alternate to both 'Sex' and 'Age'
train <- train %>% 
    mutate(Title = str_extract(Name, '[a-zA-Z]+\\.'))

table(train$Title)
#Notice that some of these Titles have only a few passengers associated with it.
#So we'd condense these titles to a small subset
#Also note that if we don't do that, and form our ML model,based on these Titles,
#then it'd be a case of 'overfit',
#as our ML algo would take (let's say a 'Don.', who as per our data,perished),
#and deduce that all the passengers who are 'Don.' actually perished,
#which may not be necessarily true, hence a bad model.

#Condense titles down to small subset
titles.lookup <- data.frame(Title = c('Capt.','Col.','Don.','Dr.','Jonkheer.','Major.','Mr.','Rev.','Sir.',
                                      'Countess.','Lady.','Mme.','Mrs.','Dona.','Ms.','Miss.','Mile.',
                                      'Master.'),
                            New.Title = c(rep('Mr.',9),
                                          rep('Mrs.',5),
                                          rep('Miss.',3),
                                          'Master.'
                                          ),
                            stringsAsFactors = FALSE
                            )
View(titles.lookup)

#Join the New.Title column
train <- train %>% left_join(titles.lookup, by = 'Title') #note: the by, has to have the same column name, in this case 'Title'

#Replace Title with newly mutated Title:
train <- train %>% mutate(Title = New.Title) %>% select(-New.Title)
#Explanation:
#mutate creates a new variable, Title, which copies all obs from New.Title,
#which is already in dataset train, but since it is the same name as of the one,
#that's present in the dataset already, hence it replaces it with this new one.
#It then does : select(-New.Title), which essentially removes New.Title from dataset.

#Alternatively, you could do the above 2 line of code in 1 line of dplyr pipeline:
train <- train %>% left_join(titles.lookup, by = 'Title') %>% mutate(Title = New.Title) %>% select(-New.Title)

#Double check the changes:
table(train$Title)
train %>% 
    filter((Sex == 'female' & (Title == 'Mr.' | Title == 'Master.')) | 
           (Sex == 'male' & (Title == 'Mrs.' | Title == 'Miss.')))        #spits out a record of a female Dr., we had assumed all 'Dr.' to be 'male'

train$Title[train$PassengerId == 797] <- 'Mrs.'  #Imputing the correct value, as per our classification of titles.

#****************************************************************************************************************************
#Lectur#4 : Impute the median of Fare, for all the 'Mr.', in the missing obs of 'Fare' variable
#1).Create a summary stats for those passengers with a Title 'Mr.' and group those stats by Pclass
mr.fare.stats <- train %>% 
    filter(Title == 'Mr.') %>% 
    group_by(Pclass) %>% 
    summarize(fare.min = min(Fare),
              fare.max = max(Fare),
              fare.mean = mean(Fare),
              fare.median = median(Fare),
              fare.sd = sd(Fare),
              fare.var = var(Fare),
              fare.IQR = IQR(Fare)
              )
View(mr.fare.stats)

#Alternatively, you could run the below code for just the same:
summary(train$Fare[train$Title == 'Mr.' & train$Pclass == 1])
summary(train$Fare[train$Title == 'Mr.' & train$Pclass == 2])
summary(train$Fare[train$Title == 'Mr.' & train$Pclass == 3])

#2).Create a 'tracking feature' for the records that had 0 Fare
train$Fare.zero <- ifelse(train$Fare == 0,'Y','N')
names(train)[14] <- 'Fare.Missing' #renaming that column
#Alternatively: names(train)[names(train) == 'Fare.zero'] <- 'Fare.Missing'

#3).Create a lookup table, which tells us the median fare of 'Mr.' in each class.
zero.fare.lookup <- train %>% 
    filter(Title == 'Mr.') %>% 
    group_by(Pclass, Title) %>% 
    summarize(New.Fare = median(Fare))

View(zero.fare.lookup)

#4).Impute the zero fares using the lookup table
train <- train %>%
    left_join(zero.fare.lookup, by = c('Pclass','Title')) %>%
    mutate(Fare = ifelse(Fare == 0,New.Fare,Fare)) %>%
    select(-New.Fare)

View(train)

#**********************************************************************************************
#Lectur 5: Impute the missing ages:
#Take a closer look at the 'Age' variable all-up
age.stats <- train %>%
    group_by(Pclass, Title) %>%
    summarize(age.min = min(Age, na.rm = TRUE),
              age.max = max(Age, na.rm = TRUE),
              age.mean = mean(Age, na.rm = TRUE),
              age.median = median(Age, na.rm = TRUE),
              age.NA.count = sum(is.na(Age))) %>% 
    arrange(Title, Pclass)
View(age.stats)

#Create 'tracking feature' for those records that originally has missing obs. for Age variable
train$Age.Missing <- ifelse(is.na(train$Age),'Y','N')
View(train)
table(train$Age.Missing, train$Title) #this splits Age.Missing by Title

#Create a lookup table
age.lookup <- age.stats %>%
    select(Pclass, Title, age.mean, age.median)
View(age.lookup)

#Impute missing ages using lookup table
train <- train %>%
    left_join(age.lookup, by = c('Pclass','Title')) %>%
    mutate(Age = ifelse(Age.Missing == 'Y',
                        (ifelse(Title == 'Miss.',age.median, age.mean)),
                        Age)) %>%
    select(-age.median, -age.mean)
View(train)
#Note: we used median for 'Miss.' coz the records for 'Miss.' was heavily skewed(coz 'Miss.' could be a female child as well as a female unmarried adult)

#************************************************************************************************
#Lecture 6:
#Look at imputed Age distribution all-up
quantile(train$Age, probs = seq(0.05, 1, 0.05)) #start at 5%, goto 100%, by the rate of 5%
#Deduction:
#If we set a cutoff for children at let's say Age18 or below, then we have about 15% of children in our data
#If we set a cutoff for old folks at let's say Age54 or more, then we have about 5% of old folks in our data

#Create a ticket lookup:
ticket.lookup <- train %>%
    group_by(Ticket) %>%
    summarize(group.count = n(),
              avg.fare = max(Fare)/n(),
              min.age = min(Age),
              max.age = max(Age),
              child.count = sum(Age<18),
              elderly.count = sum(Age>54),
              male.count = sum(Sex == 'male'),
              female.count = sum(Sex == 'female'),
              male.ratio = male.count/n(),
              female.ratio = female.count/n(),
              child.ratio = child.count/n(),
              elderly.ratio = elderly.count/n(),
              female.child.ratio = (sum(Age<18) + sum(Sex == 'female' & Age>=18))/n()
              )
View(ticket.lookup)

#Double check our work:
ticket.lookup %>% filter(Ticket == '3101295')
View(train %>% filter(Ticket == '3101295'))
#Deduction:
#a 'Mr.' may not necessarily be of Age>18, he could be anything from Age>8 onwards, which counters our cutoff for children of Age<18


#Populate training data via lookup table
train <- train %>%
    left_join(ticket.lookup, by = 'Ticket')

View(train)
View(train %>% filter(Ticket == '3101295'))

#The Payoff:- Investigate the hypothesis that travelling with children could have been detrimental/predictive
library(ggplot2)
glimpse(train)
#Setup factors
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$Title <- as.factor(train$Title)
train$Fare.Missing <- as.factor(train$Fare.Missing)
train$Age.Missing <- as.factor(train$Age.Missing)
train$group.count <- as.factor(train$group.count)

#Subset a 'passengers travelling with children' table
ticket.children <- train %>%
    filter(child.count>0)

View(ticket.children) #192 passengers were travelling with children

#Visualize the 'Passengers travelling with children'
ggplot(ticket.children, aes(x = Title, fill = Survived)) +
    geom_bar() +
    theme_dark() +
    facet_wrap(~ Pclass) +
    ggtitle('Survival rate across Pclass for ticket groups travelling with children')

ggplot(ticket.children, aes(x = Pclass, fill = Survived)) +
    geom_bar() +
    theme_dark() +
    facet_wrap(~ Title) +
    ggtitle('Survival rates for ticket groups travelling with children') +
    labs(y = 'Count of Passengers')

#Subset a 'passengers travelling alone' table
ticket.alone <- train %>%
    filter(child.count == 0)

View(ticket.alone) #891-192 = 699 passengers travelling alone

#Visualize: Passengers travelling alone
ggplot(ticket.alone, aes(x = Pclass, fill = Survived)) +
    geom_bar() +
    theme_dark() +
    facet_wrap(~ Title) +
    ggtitle('Survival rates of passengers travelling alone') +
    labs(y = 'Count of Paseengers')

ggplot(ticket.alone, aes(x = Title, fill = Survived)) +
    geom_bar() +
    theme_dark() +
    facet_wrap(~ Pclass) +
    ggtitle('Survival rates of passengers travelling alone') +
    labs(y = 'Count of Paseengers')

#********************************************************************************
#Prepping this train dataset for Kaggle Submission#2
write.csv(x = train, file = 'train.csv', row.names = FALSE)
getwd()
