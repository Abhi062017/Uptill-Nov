#Text Analysis: Using 'tm' package
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/2.codes')
sessionInfo()

#Get the text out of the list variable, to build corpus:
spam <- read.csv('spam.csv', stringsAsFactors = FALSE)
nrow(spam)
spam <- spam[1:200,]
class(spam)
str(spam)

#Now build corpus:
library(tm)
spam_corpus <- Corpus(VectorSource(spam))
spam_corpus
inspect(spam_corpus[1]) #to view the text of corpus
inspect(spam_corpus[2]) #to view the text of corpus


#Pre-Processing:
spam <- tm_map(spam_corpus,content_transformer(tolower))
spam <- tm_map(spam_corpus,removePunctuation)
spam <- tm_map(spam_corpus,removeWords, stopwords('english'))
spam <- tm_map(spam_corpus,removeNumbers)
spam <- tm_map(spam_corpus,stripWhitespace)

#Invoking wordcloud:
library(wordcloud)
spam
inspect(spam[1])
inspect(spam[2])
wordcloud(spam, colors = rainbow(100),
          random.order = FALSE,
          max.words = 400
          )
