getwd()
setwd('C:/Users/abhi/Downloads/Jobs')
install.packages('stringr')
install.packages("knitr")
library(stringr)
library(knitr)
library(ggplot2)

#Feature of ggplot()
data('iris')
head(iris)
ggplot(iris, aes(x=Sepal.Length, fill=Species))+geom_histogram(binwidth = 1)+theme_bw()


library(dplyr)
#Feature of dplyr()
iris.stats <- iris %>%    #%>% this pipes data to the next operation
  filter(Species =='setosa' |
         Species == 'virginica') %>%
  group_by(Species) %>%
  summarize(Sepal_mean = mean(Sepal.Length),
            Sepal_min = min(Sepal.Length),
            Sepal_max = max(Sepal.Length),
            Sepal_median = median(Sepal.Length),
            Sepal_sd = sd(Sepal.Length))

View(iris.stats)

#Code to get the list of all the built-in datasets
library(help="datasets")

#**************************************************************************
# DataScience-dplyr handson
library(dplyr)    #loading dplyr in memory
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/DataScience - dplyr')
install.packages("hflights")    #installing the 'hflights' package/dataset
library(hflights) #loading the package/dataset

View(hflights)
head(hflights)
ncol(hflights)
nrow(hflights)
data(hflights)
str(hflights)
glimpse(hflights)   #dplyr's way of str(hflights)
# creating a local dataframe, from the package/dataset, so it can print nicely.
hflights
flights <- tibble::as_tibble(hflights)
flights     #see the difference, as compared to hflights
print(flights, n=20)  #prints 20 rows
n_distinct(flights$Dest) #prints the total no. of distinct values of a variable/column
data.frame(head(flights)) #prints all columns

# filter: Keep rows matching criteria

* Base R approach to filtering forces you to repeat the data frames name
* dplyr approach is simpler to write and read
* Command structure (for all dplyr verbs):
    * first argument is a data frame
    * return value is a data frame
    * nothing is modified in place
* Note: dplyr generally does not preserve row names

  # base R approach to view all flights on January 1
  flights[flights$Month==1 & flights$DayofMonth==1,]

  # dplyr approach
  # dplyr verb#1: filter
  # note: you can use comma or ampersand to represent AND condition
  filter(flights, Month==1, DayofMonth==1)

  # note, you can use the | for OR condition, or the %in% infix operator for OR
  filter(flights, UniqueCarrier=='AA' | UniqueCarrier=='UA')
  filter(flights, UniqueCarrier %in% c('AA','UA'))
  

  # dplyr verb#2: select
  # select: Picks columns by name
  * Base R approach is awkward to type and to read
  * dplyr approach uses similar syntax to `filter`
  * Like a SELECT in SQL
  
  # base R approach to select DepTime, ArrTime, and FlightNum columns
  flights[,c('DepTime','ArrTime','FlightNum')]
  # dplyr approach
  select(flights, DepTime, ArrTime, FlightNum)
  # use colon to select multiple contiguous columns, and use `contains` to match columns by name
  select(flights, Year:DayofMonth, contains('Taxi'), contains('Cancel'))
  
# "Chaining" or "Pipelining"
  * Usual way to perform multiple operations in one line is by nesting
  * Can write commands in a natural order by using the `%>%` infix operator (which can be pronounced as "then")
  
  # nesting method to select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes
  filter(select(flights, UniqueCarrier, DepDelay), DepDelay>60)
  
  # chaining method : this is more readable
  flights %>%
    select(UniqueCarrier, DepDelay) %>%
    filter(DepDelay>60)
  
# Use of chanining method outside dplyr
  # Euclidean Distance, with base R method
  num1 <- 1:5
  num2 <- 2:6
  sqrt(sum((num1-num2)^2))
  
  # Euclidean Distance, with chaining method
  num1 <- 1:5
  num2 <- 2:6
  (num1-num2)^2 %>% sum() %>% sqrt()
  
# dplyr verb#3 : arrange
    # It reorders the rows
    # base R approach to select UniqueCarrier and DepDelay columns and sort by DepDelay
    flights[order(flights$DepDelay), c('UniqueCarrier','DepDelay')]
    # dplyr's arrange approach
    flights %>%
      select(UniqueCarrier, DepDelay) %>%
      arrange(DepDelay)
      #by default, it's in ascending order. Use desc, to arrange it in descending order
      flights %>%
        select(UniqueCarrier, DepDelay) %>%
        arrange(desc(DepDelay))

# dplyr verb#4: mutate
  # It adds new variables
    * Create new variables that are functions of existing variables
  
  # base R approach to create a new variable Speed (in mph)
  # note: Speed (in mph) = (Distance/AirTime)*60
  flights$Speed <- (flights$Distance/flights$AirTime)*60
  str(flights)
  flights[, c('Distance', 'AirTime','Speed')]
  
  # dplyr's mutate approach: Note, this just prints the result, but does not store it
  flights %>%
    select(Distance, AirTime) %>%
    mutate(Speed = Distance/AirTime*60)
  # the below code mutates and stores the mutated variable in the dataset
  flights <- flights %>% mutate(Speed=Distance/AirTime*60)
  
  
# dplyr verb#5 : summarise
    # It reduces the variables to values
    * Primarily useful with data that has been grouped by one or more variables
    * `group_by` creates the groups that will be operated on
    * `summarise` uses the provided aggregation function to summarise each group
    # calculate the average arrival delay to each destination
    flights %>%
    group_by(Dest) %>%
    summarise(avg_arrival_delay = mean(ArrDelay, na.rm=TRUE))
    
    # summarise_each : allows you to apply the same summary function to multiple columns at once
    flights %>%
      group_by(Dest) %>%
      summarise_each(funs(mean), Distance, AirTime)
    
      #Since the AirTime has some NA's, hence the following
      test10 <- flights[, c('Dest', 'Distance', 'AirTime')]
      test10$AirTime[which(is.na(test10$AirTime))] <- 0
      str(test10)
      #applying the summarise_each
      test10 %>%
        group_by(Dest) %>%
        summarise_each(funs(mean), Distance, AirTime)


# Helper functions
* `n()` counts the number of rows in a group
* `n_distinct(vector)` counts the number of unique items in that vector
  #1. for each day of the year, count the total number of flights and sort in descending order
  flights %>%
    group_by(Month, DayofMonth) %>%
    summarise(flight_count = n()) %>%
    arrange(desc(flight_count))
    #note: we can use tally(), whenever summarise is used with the helper function n()
    flights %>%
      group_by(Month, DayofMonth) %>%
      tally(sort=TRUE)  #note: sort=TRUE is same as 'descing order'
  #2. for each destination, count the total number of flights and the number of distinct planes that flew there
  flights %>%
    group_by(Dest) %>%
    summarise(flights_count = n(), ditinct_planes = n_distinct(TailNum))

  * Grouping can sometimes be useful without summarising
  # for each destination, show the number of cancelled and not cancelled flights
  flights %>%
    group_by(Dest) %>%
    select(Cancelled) %>%
    table() %>%
    head()

# Other useful convenience functions:
flights %>% sample_n(10, replace = FALSE)
flights %>% sample_frac(0.999, replace = FALSE)
glimpse(flights)    #it's a better alternate to str(flights)

