getwd()
setwd('C:\\Users\\Abhishek\\Downloads\\Jobs\\New Job\\2.codes')
#****************************************
data("anscombe")
str(anscombe)
fix(anscombe) #allows editing on the fly
View(anscombe)
library(psych)
pairs.panels(anscombe)

#****************************************
rm(list = ls()) #removes all the objects of the current workspace
abc<- read.csv(file.choose(), stringsAsFactors = F, header = T)
write.csv(abc, file = 'gold.doc', row.names = F) #write the dataset in any format

#****************************************
data(UCBAdmissions)
class(UCBAdmissions) #it's a table, not a dataframe
View(UCBAdmissions)
margin.table(UCBAdmissions,1) #Admit
margin.table(UCBAdmissions,2) #Gender
margin.table(UCBAdmissions,3) #Dept
margin.table(UCBAdmissions) #total records
#same as below:
apply(UCBAdmissions, 1, sum) #1 for 1st variable
apply(UCBAdmissions, 2, sum) #2 for 2nd variable
apply(UCBAdmissions, 3, sum) #2 for 3rd variable
#other example
a <- as.matrix(1:25)
margin.table(a, 1) #1 for rows
margin.table(a, 2) #2 for columns...., note,it is same as: apply(a, 2, sum)
prop.table(a, 2) #spits out fractions of each value (2 for columns)

#***************************************************************************
UCB_dept <- margin.table(UCBAdmissions, 3)
View(UCB_dept)
plot(UCB_dept, type = 'l', ylim = c(0,1000)) #line
plot(UCB_dept, type = 'p', ylim = c(0,1000)) #points
plot(UCB_dept, type = 'o', ylim = c(0,1000)) #overplotted
colors()
barplot(UCB_dept, ylim = c(0,1000), col = 'slategray3')
barplot(UCB_dept, ylim = c(0,1000), col = colors()[102]) #using index for colors