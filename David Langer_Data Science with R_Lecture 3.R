# Note: Random Forest algorithm can't handle factors with more than 32 levels
# Hence, if you are to use Random Forest algo, make sure the factor variables,
# that you are using, each has levels <= 32, and no nulls
# also if you have continuous variables (int/numeric/character), they
# should also not have any null values. These are the limitations of Random Forest.

getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/')

# Take a look at the ticket variable
str(data.combined$Ticket)
# We deduce that it is a factor of 929 levels.
  # We can convert it into string, as based on the huge no. of levels,
  # ticket really isn't a factor variable, it is a string.
  data.combined$Ticket <- as.character(data.combined$Ticket)
  data.combined$Ticket[1:20] # spits out the first 20 values

# There's no immediately apparent structure in the data. Lets see if we can find some.
# We'll start with taking a look at just the first char of each.
ticket.first.char <- ifelse(data.combined$Ticket == "", " ",substr(data.combined$Ticket,1,1))
# note: in the above code, the first condition of ifelse is reduntant/unnecessary,
# as there're no tickets (empty strings) in it. But since we have to get the second
# result, hence we play along
ticket.first.char[1:100]
unique(ticket.first.char)
  # Now we deduce from the unique values of ticket variable,
  # that we can actually convert the dtype of 'Ticket' to factors,
  # as they are only around 17 unique values

# Adding the 'ticket.first.char' to the dataset and changing the dtype to factor
names(data.combined)
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# Now, since it's a factor variable, we can run some analysis/visualization
# First a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  xlab('ticket.first.char') +
  ylab('total count') +
  ggtitle('survivability by ticket.first.char') +
  labs(fill = 'Survived')

  # Ticket seems that it might be predictive (people perish or survived),
  # Let's drill it down a bit, by introducing facet_wrap to it
  ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) +
    xlab('ticket.first.char') +
    ylab('total count') +
    ggtitle('survivability by ticket.first.char') +
    labs(fill = 'Survived')
  
  # Not productive enuff. Let's introduce the combo of Pclass and title in facet_wrap
  ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass + title) +
    xlab('ticket.first.char') +
    ylab('total count') +
    ggtitle('survivability by ticket.first.char') +
    labs(fill = 'Survived')
  
# Discarding 'Ticket' variable, let's move to 'Fare' variable
str(data.combined)
summary(data.combined$Fare) #Mean is heavily skewed vis-a-vis Median, bcoz of the 'Max' fare.
length(unique(data.combined$Fare)) # that's a lot of values, don't converted to factors.

  # Can't make fare a factor, treat it as is (numeric) and visualize
  ggplot(data.combined, aes(x = Fare)) +
    geom_bar(width = 5) +
    ggtitle('Combined Fare Distribution') +
    theme_classic() +
    xlab('Fare') +
    ylab('Count')
  
  # Let's see if Fare has predictive power
  ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
    geom_bar(width = 5) +
    ggtitle('Survivability by Fare') +
    theme_classic() +
    facet_wrap(~Pclass + title)
    xlab('Fare') +
    ylab('Count') +
    labs(fill = 'Survived')
  
  # Deduction : nothing stands out for 'Fare' to be considered a variable worthy of predicting.

# Since,'Fare' was no use, we'd move to the next variable in the dataset, 'Cabin'.
str(data.combined$Cabin) #it's a factor of 187 levels
unique(data.combined$Cabin) # spits out 187 unique values
length(unique(data.combined$Cabin))

  # Let's change 'Cabin' variable's dtype to character,
  # as a factor of 187 values is of little use.
  data.combined$Cabin <- as.character(data.combined$Cabin)
  data.combined$Cabin[1:100] # notice the amount of blank 'Cabin' values

  # Working on the 'Cabin' variable a bit more, by assigning 'U' to blank records
  data.combined[which(data.combined$Cabin == ""), 'Cabin'] <- 'U'
    # the above can be written as below:
    data.combined$Cabin[which(data.combined$Cabin == "")] <- 'U'
  data.combined$Cabin[1:30]
  
  # Notice, the 'Cabin' values have a distinct naming pattern,
  # (first part is character then followed by numbers)
  # Since, the 'Cabin' position tells where a passenger was on the ship,
  # we can use this knowledge further :
    
    # Take a look at the first char of the 'Cabin' name,and convert it to a factor
    Cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
    Cabin.first.char[1:30]
    levels(Cabin.first.char) # 9 levels, including 'U' (Unassigned record)
    str(Cabin.first.char) # factor with levels = 9
    # Now, we have a factor variable with levels <= 32
    # (as is the limit for a factor variable for it to be considered for Random Forest algo)
    
    # Cramming this useful variable to the dataset
    data.combined$Cabin.first.char <- Cabin.first.char
    
    # High-level plot
    ggplot(data.combined[1:891,], aes(x = Cabin.first.char)) +
      geom_bar() +
      xlab('Cabin.first.char') +
      ylab('Count')
    
    # Adding the fill
    ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
      geom_bar() +
      xlab('Cabin.first.char') +
      ylab('Count')
    
    # Could have some predictive power, drill further
    ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
      geom_bar() +
      facet_wrap(~Pclass) +
      xlab('Cabin.first.char') +
      ylab('Count')
    
    # Let's fine grain it further as it's still not discernable enuff
    ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
      geom_bar() +
      facet_wrap(~Pclass + title) +
      xlab('Cabin.first.char') +
      ylab('Count')
    
    # 'Cabin' doesn't standout as a potential predictor, yet.
    # Let's check upon the folks with multiple Cabins
    data.combined$Cabin[1:50]
    data.combined$Cabin.multiple <- ifelse(str_detect(data.combined$Cabin, " "), 'Y','N')
    data.combined$Cabin.multiple[1:100]
    str(data.combined$Cabin.multiple) #it's a character variable
    # Since, we have reduced the values of 'Cabin.multiple' to 2,
    # we can make it a factor instead, and cram it in our dataset
    data.combined$Cabin.multiple <- as.factor(data.combined$Cabin.multiple)
    
    # Now, using the factor variable to plot a ggplot
    ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
      geom_bar() +
      facet_wrap(~Pclass + title) +
      ggtitle('Survivability by multiple Cabins') +
      xlab('Cabin.multiple') +
      ylab('Count') +
      labs(fill = 'Survived')
    # Nothing discernible deduced with 'Cabin.multiple' variable.
    # So we can pass on it for the time being.

# Let's move to the final variable, 'Embarked'
str(data.combined$Embarked) #it's already a factor of level = 4
levels(data.combined$Embarked)
length(unique(data.combined$Embarked))

  # Using it to plot a ggplot
  ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
    geom_bar() +
    ggtitle('Survivability by Embarked points') +
    xlab('Embarked points') +
    ylab('Counts') +
    labs(fill = 'Survived')
  
  # facet_wrap by Pclass
  ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
    geom_bar() +
    ggtitle('Survivability by Embarked points') +
    facet_wrap(~Pclass) +
    xlab('Embarked points') +
    ylab('Counts') +
    labs(fill = 'Survived')
  
  # facet_wrap by combo of (Pclass, title)
  ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
    geom_bar() +
    ggtitle('Survivability by Embarked points') +
    facet_wrap(~Pclass + title) +
    xlab('Embarked points') +
    ylab('Counts') +
    labs(fill = 'Survived')
  # Nothing discernible still from 'Embarked' variable, so we'd pass on it.