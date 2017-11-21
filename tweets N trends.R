#Text Analysis: Twitter Mining
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/2.codes')
sessionInfo()

#Installing relevant package
install.packages(twitteR)

#From 'https://apps.twitter.com', register API(Application Program Interface)
api_key <- 'Rfkmn4ZRCfZgPw88coRqrh7ih'
api_secret <- '4rCkUjrOwIKYLg0YOtof4RDCLIiXF6rqQ3yhOMH3BbSHSZDNrp'
access_token <- '3036648858-w9IYmvJKHyxC6jfeAuR2wm7ihOuEx49jfkrvRMt'
access_token_secret <- '4btfgLrSZ7dkggakeL1gdYx44pN5R41Jxg6aRRVjK7s9K'

library(twitteR)
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#Populate the search variable:
tweets_ESPNcricinfo <- searchTwitter('ESPNcricinfo', n=20, lang = 'en')
tweets_ESPNcricinfo
class(tweets_ESPNcricinfo)
str(tweets_ESPNcricinfo)

#Get the text out of the list variable, to build corpus:
tweets_ESPNcricinfo_text <- sapply(tweets_ESPNcricinfo, function(x) x$getText())
class(tweets_ESPNcricinfo_text)
str(tweets_ESPNcricinfo_text)

#Now build corpus:
library(tm)
tweets_ESPNcricinfo_text_corpus <- Corpus(VectorSource(tweets_ESPNcricinfo_text))
tweets_ESPNcricinfo_text_corpus
inspect(tweets_ESPNcricinfo_text_corpus[1]) #to view the text of corpus

#Pre-Processing:
tweets_ESPNcricinfo_text_corpus <- tm_map(tweets_ESPNcricinfo_text_corpus,
                                          content_transformer(tolower))
tweets_ESPNcricinfo_text_corpus <- tm_map(tweets_ESPNcricinfo_text_corpus,
                                          removePunctuation)
tweets_ESPNcricinfo_text_corpus <- tm_map(tweets_ESPNcricinfo_text_corpus,
                                          removeWords, stopwords('english'))
tweets_ESPNcricinfo_text_corpus <- tm_map(tweets_ESPNcricinfo_text_corpus,
                                          removeNumbers)
tweets_ESPNcricinfo_text_corpus <- tm_map(tweets_ESPNcricinfo_text_corpus,
                                          stripWhitespace)
tweets_ESPNcricinfo_text_corpus <- tm_map(tweets_ESPNcricinfo_text_corpus,
                                          removeWords, c('cricket','india','India','new zealand','New Zealand'))

#Invoking wordcloud:
library(wordcloud)
tweets_ESPNcricinfo_text_corpus
inspect(tweets_ESPNcricinfo_text_corpus[1])
wordcloud(tweets_ESPNcricinfo_text_corpus)
