#List of Packages:
browseURL('https://cran.r-project.org/web/views/')
browseURL('https://cran.r-project.org/web/packages/available_packages_by_name.html')
browseURL('https://crantastic.org')

#unload & remove the package:
detach('package:babynames', unload = T)
remove.packages('babynames')

library() #shows installed packages
search() #shows currently loaded packages
sapply(iris, class) #shows class of each variable

install.packages("missForest", dependencies = T) #prodNA(x, noNA=0.1)
library(missForest)
iris.miss <- prodNA(iris, noNA = 0.1)
