# remove a dataset
rm(flights)

#install package
install.packages('nycflights13')

# load packages
suppressMessages(library(dplyr))
library(nycflights13)
flights <- nycflights[,]

# Using dplyr's select() to pick columns
flights %>% select(carrier, flight)

# Using dplyr's minus sign to hide columns
flights %>% select(-month, -day)

# Hide a range of columns
flights %>% select(-(dep_time:arr_delay))

# Hide any column with a matching name
flights %>% select(-contains("time"))


# rename() to rename the columns. All the columns not mentioned are kept
flights %>% rename(tail = tailnum)


mtcars %>% head()
mtcars <- mtcars %>% tibble::rownames_to_column('model')%>% head()
mtcars %>% group_by(cyl) %>% summarise(count = n())
mtcars %>% count(cyl)

data_frame(a=1:6, b=a^2, c='string', 'd+e'=1) %>% glimpse()
data.frame(a=1:6, c='string', 'd+e'=1) %>% glimpse()

a <- data_frame(color = c('green', 'yellow', 'red'), num=1:3)
b <- data_frame(color = c('green','yellow','pink'), size=c('S','M','L'))
glimpse(a)
glimpse(b)

inner_join(a,b)
full_join(a,b)
left_join(a,b)
right_join(a,b)
semi_join(a,b)
anti_join(a,b)

"""Sometimes, matching variables don't have identical names.
In those cases, use 'by = ' in the join statement to retrieve the correct result"""

b <- b %>% rename(b_color = color)  #changing the variable name
inner_join(a,b, by = c('color' = 'b_color'))
