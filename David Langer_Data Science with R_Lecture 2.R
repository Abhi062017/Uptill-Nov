getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R')

# Distribution of males to females across 'train' and 'test' datasets
table(data.combined$Sex)

# ggplot for the Survival rate of passengers based on Pclass and Sexes
ggplot(data.combined[1:891, ], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.8) +
  facet_wrap(~Pclass) +
  ggtitle('Survival rate based on Pcalss and Sexes') +
  xlab('Sexes') +
  ylab('Total Count') +
  labs(fill = 'Survived')

summary(data.combined$Age)
summary(data.combined[1:891, 'Age']) #spits out the summary of train dataset's 'Age' variable

# ggplot to plot the survival rates broken down by Sex,Pclass and Age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~Sex + Pclass) +
  ggtitle("Survival rates based on Sex, Pclass and Age")
xlab("Ages") +
  ylab("Total Count")

# Validate that "Master." is a good proxy for male children
View(data.combined)
boys <- data.combined[which(data.combined$title == 'Master'),]
summary(boys$Age) #Successfully infered that 'Master' is indeed a good proxy for male kids
View(boys)

# Let's examine 'Miss.'
Misses <- data.combined[which(data.combined$title =='Miss.'),] #Since we already have misses, hence Misses.
str(Misses)
View(Misses)
tail(Misses)
summary(Misses$Age)

# ggplot on Survived Misses
ggplot(Misses[Misses$Survived != "None",], aes(x = Age, fill = Survived)) + # Misses$Survived != 'None', because data.combined has 'None' as it's Survived column's entries
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass) +
  ggtitle('Survival rates by Pclass and Age') +
  xlab('Ages') +
  ylab('Total COunt')

#  OK, appears female children may have different survival rate,
# could be a candidate for feature engg later
Misses.Alone <- Misses[which(Misses$SibSp == 0 & Misses$Parch ==0),] # Misses.Alone = traveling alone(without spouse/siblings/parents/children)
summary(Misses.Alone$Age) # Infer that 25% are 21 or less, but 75% are 32.5 or older, hence very few are children of these

# Count the no. of female children
length(which(Misses.Alone$Age <=14.5)) 
  # we picked 14.5 as the max age coz that is what is with boys
  # we also infer that only 4 are female children from a total 150 observations of Misses.Alone
  # So, we'd say almost all of 'Misses.Alone' were adult females, we are 97.3% right with that hypothesis


# Move on to the SibSp variable and summarize it
summary(data.combined$SibSp) 
  # Inference :
  # Median=0 : Half the passengers were traveling without Siblings/Spouse
  # Mean = 0.49, which is close to 0, also it is not an integer(as 'SibSp' have to be an integer value)
  # We can use this variable 'SibSp' as a factor(as the Max is 8, so it'd be from 1 to 8)

  # Can we treat the 'SibSp' as a factor?
  length(unique(data.combined$SibSp)) #returns '7' unique values, hence can be treated as factor
  
  # Coverting the dtype
  data.combined$SibSp <- as.factor(data.combined$SibSp)
  
  # Now we can plot the ggplot using SibSp
  ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
    geom_bar(width = 1) +
    ggtitle('Survival rates by SibSp count and Pclass') +
    facet_wrap(~Pclass + title) +
    xlab('Siblings and Spouse') +
    ylab('Total Count') +
    ylim(0,350) +
    labs(fill = 'Survived')

# Similar to 'SibSp', we can consider 'Parch' as a factor
summary(data.combined$Parch) #follows a trend very similar to 'SibSp', hence can be considered a factor

  length(unique(data.combined$Parch)) #returns '8' unique calues
  data.combined$Parch <- as.factor(data.combined$Parch) #dtype conversion
  
  # Plotting ggplot using Parch
  ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
    facet_wrap(~Pclass + title) +
    geom_bar(width = 1) +
    xlab('No. Of Parents and Children') +
    ylab('Total Count') +
    ggtitle('Survival rates of Parch variable by Pclass and title') +
    labs(fill = 'Survived')

# Let's try some feature engg. How about creating a family size feature!
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$family_size <- c(temp.Parch+temp.SibSp+1)
max(data.combined$family_size)
unique(data.combined$family_size)
View(data.combined) # we notice that the values of 'family_size' are single digit integers
# Hence converting the dtype of 'family_size'
data.combined$family_size = as.factor(data.combined$family_size)
str(data.combined)

  # Invoking ggplot on family_size
  ggplot(data.combined[1:891,], aes(x = family_size, fill = Survived)) +
    geom_bar(width = 1) +
    facet_wrap(~Pclass + title) +
    ggtitle('Survival rates of family_size variable by Pclass and Titles') +
    xlab('family_size') +
    ylab('Count') +
    labs(fill = 'Survived')
