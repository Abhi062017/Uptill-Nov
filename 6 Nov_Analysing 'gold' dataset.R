getwd()
setwd('C:\\Users\\Abhishek\\Downloads\\Jobs\\New Job\\2.codes')
#**********************************************************************#
#read dataset
gold <- read.csv(file.choose(), stringsAsFactors = F)
str(gold)
summary(gold)
head(gold)
gold

#casting relevant dtypes:
gold$Date <- as.Date(gold$Date)
gold$Open <- as.numeric(gold$Open)
gold$High <- as.numeric(gold$High)
gold$Low <- as.numeric(gold$Low)
gold$Close <- as.numeric(gold$Close)
gold$Adj.Close <- as.numeric(gold$Adj.Close)
gold$Volume <- as.numeric(gold$Volume)

summary(gold)
head(gold)
sum(is.na(gold)) #returns the no.of NA's

#Analysing and plotting NA's
install.packages("mice", dependencies = T) #to use md.pattern()
install.packages("VIM", dependencies = T) #to plot NA pattern
library(mice)
md.pattern(gold) #67 obs have no NA's.
library(VIM)
aggr_plot <- aggr(gold, col=c("green","brown"),
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(gold), cex.axis=.7,
                  gap=6, ylab=c("Histogram of missing data Pattern", "Pattern")
                  ) #98.5% of the records are without NA's, just 1.5% have NA.

aggr_plot
aggr_plot[['percent']]
aggr_plot[['count']]
aggr_plot[['missings']]

#Imputing NA's
gold_noNA <- kNN(data = gold, variable = c(2,3,4,5,6,7), k=67)
gold_noNA <- gold_noNA[1:7] #filtering just the required variables
sum(is.na(gold_noNA)) #no NA's
tail(gold_noNA)
summary(gold_noNA)

  #Using subset() to filter variables
  gold2 <- kNN(gold) #notice, default 'k'=5, 'variable'=all with NA's
  gold2 <- subset(gold2, select = c(1:7))
  rm(gold2)
