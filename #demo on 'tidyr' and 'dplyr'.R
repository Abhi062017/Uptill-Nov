#demo on 'tidyr' and 'dplyr'
data("iris")

library(dplyr)
mini_iris <- iris[c(1,51,101),] #when you know the values for 'Species'
mini_iris2 <- iris %>% group_by(Species) %>% slice(1) #spits out 1st record of every 'Species'
mini_iris3 <- iris %>% group_by(Species) %>% slice(c(1,n()))  #this spits out the 1st and last record of the 'Species'
tbl_df(mini_iris)
tbl_df(mini_iris2)
tbl_df(mini_iris3)

library(tidyr)
mini_iris_tidy <- gather(mini_iris, 
                         key = 'flower_attribute',
                         value = 'measurements',
                         -Species)
tbl_df(mini_iris_tidy)
tbl_df(mini_iris)

