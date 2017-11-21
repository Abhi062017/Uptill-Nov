#Multivariate Analysis: wine dataset
getwd()
wine <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',
                   sep = ',')
write.csv(wine, file = 'wine.csv', row.names = F)
rm(wine)
wine <- read.csv('wine.csv', stringsAsFactors = F, header = T)


View(wine)
str(wine)
summary(wine)
pairs.panels(wine)

install.packages("car")
library(car)
scatterplotMatrix(wine[2:6])
pairs.panels(wine[2:6]) #deduction: V4,V5 share strong relation

pairs.panels(wine[4:5])
cor(wine$V5, wine$V4)

plot(wine$V4, wine$V5)
text(wine$V4, wine$V5, wine$V1, cex = 0.5, pos = 4, col = 'brown')
#Deduction: wines from 2(V1==2) seem to have lower values of V4,
#compared to the wines of 1(V1==1).

install.packages("RColorBrewer")
library(RColorBrewer)

names <- c('V2','V3','V4','V5','V6')
wine_list <- c(wine$V2, wine$V3, wine$V4, wine$V5, wine$V6)

sapply(wine[2:14], mean)
sapply(wine[2:14], sd)

count(wine %>% filter(V1==1))
count(wine %>% filter(V1==2))
count(wine %>% filter(V1==3))

cultivar2 <- wine[wine$V1==2,]
sapply(cultivar2[2:14], sd)
cor.test(wine$V4, wine$V5, method = c("kendall"))



mosthighlycorrelated <- function(my_df,n_report)
{
  # find the correlations
  cormatrix <- cor(my_df)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=n_report)
}

mosthighlycorrelated(wine[2:14], 10)

library(dplyr)

cormatrix <- cor(wine[2:14])
diag(cormatrix) <- 0
cormatrix[lower.tri(cormatrix)] <- 0
fm <- as.data.frame(as.table(cormatrix))
names(fm) <- c("First.Variable", "Second.Variable","Correlation")

head(fm %>% filter(Correlation == max(Correlation)))

head(fm[order(abs(fm$Correlation),decreasing=T),],n=n_report)

str(fm)
