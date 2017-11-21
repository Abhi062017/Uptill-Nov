#Data Wrangling with 'tidyr' & 'dplyr'

#'tidyr' functions used:
  #gather() : Helps making observations from variables
  #spread(): Helps making variables from observations
  #separate(): Helps split columns
  #unite(): Helps merge columns

#'dplyr' functions used:
  #select(): Helps extract existing variables
  #filter(): Helps extract existing observations
  #mutate(): Helps derive new variables(from existing variables)
  #arrange(): Helps arrange the data in asc/desc
  #group_by(): Helps group the data
  #summarize(): Helps analysing the data

getwd()

#***********************************************************************************
#'tidyr':
#***********************************************************************************

library(devtools)
devtools::install_github('rstudio/EDAWR')

library(EDAWR)
data("cases")
View(cases)

library(dplyr)
glimpse(cases)
tbl_df(cases)

#changing the structure of the 'cases' dataset for effective data wrangling
install.packages("tidyr")
library(tidyr)
cases_tidy <- gather(cases, 'year','n', 2:4) #notice the 'dim' change from cases
#note: you use 'gather' when you notice that you have columns that are not variables
View(cases_tidy)
tbl_df(cases_tidy)


data("pollution")
glimpse(pollution)
tbl_df(pollution) #notice 'city' has multiple records for each city
#changing the sructure of the 'pollution' dataset for effective data wrangling
pollution_tidy <- spread(pollution, size, amount)
#note: It spreads a key-value pair across multiple columns.
tbl_df(pollution_tidy)


data("storms")
tbl_df(storms) #notice, we can split 'date' variable into 3 variables
#changing the sructure of the 'storms' dataset for effective data wrangling
storms_tidy <- separate(storms, date, into = c('Year','Month', 'Day') , sep = '-')
tbl_df(storms_tidy)

#reversing the changes using tidyr's 'unite'
storms_untidy <- unite(storms_tidy, col = date, c('Year','Month','Day'), sep = '.')
tbl_df(storms_untidy)


#***********************************************************************************
#'dplyr':
#***********************************************************************************
pollution %>%  group_by(city) %>% summarize(Mean = mean(amount), Sum = sum(amount), N = n())
pollution %>%  summarize(Mean = mean(amount), Sum = sum(amount), N = n())
pollution %>% group_by(city) %>% summarise(Sum = sum(amount))
pollution %>% ungroup(city) %>% summarise(Sum = sum(amount)) #compare with group_by()


#'dply' on 'tb' dataset
data(tb)
tbl_df(tb)

a <- tb
#Filter dataset into non-NA's

  #Ways to filter NA's:
  #For vectors vec_name[!is.na(vec_name)]
  #For data frames use complete.cases(data_frame_name) or in dplyr syntax.
  #data_frame_name %>% filter(complete.cases(.))

non.NA <- a %>% filter(complete.cases(.))
nrow(non.NA <- a %>% filter(complete.cases(.)))
tbl_df(non.NA) #notice, it has removed all the records that have even 1 NA
tbl_df(a)

#So we'd impute 0's for NA's from the original dataset instead,
#as moving ahead with our 'non-NA' dataset, may cause a loss of records (420 of them)
rm(non.NA)
a[is.na(a)] <- 0 #simple code to replace NA's with 0
tbl_df(a)

a %>% distinct(country)
a %>% distinct(year)

a %>% group_by(country, year) %>% summarize()
pollution
pollution %>% ungroup(city) %>% summarise(Sum = sum(amount))
