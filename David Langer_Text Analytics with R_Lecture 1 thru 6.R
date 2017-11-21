# Text Analytics with R_Lecture 01: *reading the dataset -to- visualizing it*
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/')

# (I): Reading the file/dataset
spam.raw <- read.csv('spam.csv', stringsAsFactors = FALSE)
View(spam.raw)

  # The first step, as always, is to explore data.
    # Changing the column names from 'type'/'text' to 'Label'/'Text'
    names(spam.raw) <- c('Label', 'Text')
    str(spam.raw)
    
    # Since, 'Label' is a char of just 2 values (ham/spam),
    # we'd make it a factor variable.
    spam.raw$Label <- as.factor(spam.raw$Label)
    
    # Check data to see if there are missing values
    length(which(!complete.cases(spam.raw))) # we deduce : no missing values
    
    # Let's take a look at the distribution of class Labels (ham/spam)
    table(spam.raw$Label) 
    prop.table(table(spam.raw$Label)) # 13.4% spams, 86.5% hams
    
    # Let's get a feel for the distribution of 'Text' lengths
    spam.raw$Text_Length <- nchar(spam.raw$Text)
    View(spam.raw)
    summary(spam.raw$Text_Length) # 75% of the Text_Length is <= 122 characters
    
    # Visualize by adding segmentation for ham/spam:
    library(ggplot2)
    ggplot(spam.raw, aes(x = Text_Length, fill = Label)) +
      geom_histogram(binwidth = 4) +
      ggtitle('Distribution of Ham/Spam by Text Length') +
      theme_dark() +
      xlim(0,500) +
      xlab('Text Length') +
      ylab('Text Count')

#************************************************************************************************************#
    
# (II): Text Analytics with R_Lecture 02&03: *Pre-processing -to- dfm building*
  # Invoking ML on our dataset:
    # At a bare minimum, we need to split our data into a training set and a 
    # test set. In a true project, we would want to use a three-way split
    # of : training, validation, and test.
    
    # As, we know that, our data has non-trivial class imbalance,
    # so we'll use the mighty caret package to create a random train/test split
    # that ensures the correct ham/spam class label proportions,
    # (i.e) we'll use caret for a random stratified split.
    
    library(caret)
    help(package = 'caret')
    
    # Use Caret to create a 70%/30% stratified split.
    # And, set the random seed for reproducibility.
    set.seed(32984)
    indices <- createDataPartition(spam.raw$Label, times=1, p=0.7, list=FALSE)
    3902/5574  # validating our splits (indices / total observations in spam.raw)
    
    # Preparing our datasets : train/test
    train <- spam.raw[indices, ] # contains 70% of spam.raw
    test <- spam.raw[-indices, ] # contains 30% of spam.raw
    # this stores data excluding indices.
    # It'll be the dataset to test our train model up against.
    
    
    # Verify proportions of the train/test datasets.
    table(train$Label)
    table(test$Label)
    prop.table(table(train$Label)) # compare with: prop.table(table(spam.raw$Label))
    prop.table(table(test$Label)) # compare with: prop.table(table(spam.raw$Label))
    
    # We gon use 'quanteda' for the text analysis.
    library(quanteda)
    help(package = 'quanteda')
    
      # Step.1: Tokenize the text messages
        # Note: It returns a list.
        train.tokens <- tokens(train$Text, what = 'word',
                               remove_numbers = TRUE, remove_punct = TRUE,
                               remove_symbols = TRUE, remove_hyphens =  TRUE)
        
        train.tokens[[358]] #compare this with below:
        train$Text[358]
        
      # Step.2: Lower case the tokens
        train.tokens <- tokens_tolower(train.tokens)
        train.tokens[[358]]
        
      # Step.3: Apply quanteda's 'tokens_select()' function, and,
        # use quanteda's built in stopword() list for English Text,
        # which is a list of pre-defined/pre-stored set of words,
        # that are not needed to be placed in the code.
        train.tokens <- tokens_select(train.tokens,
                                      stopwords(),
                                      selection = 'remove')
        
        length(stopwords()) #175 is the total no of built in stopwords,
        train.tokens[[358]]
        
      # Step.4: Perform quanteda's 'tokens_wordstem()' function:
        train.tokens <- tokens_wordstem(train.tokens, language = 'english')
        train.tokens[[358]]
        
      
      # Step.5 : Once done with the pre-preocessing of the text,
        # it's time we created our first 'Bag-Of-Words' Model.
        # dfm: document-frequency matrix / document-feature matrix
        # dfm, at the core of it, is a dataframe, where each row represents a document
        # and each column represents a distinct token.
        train.tokens.dfm <- dfm(train.tokens, tolower = FALSE) 
        
        # Now, creating a matrix of the dfm, to view it.
        train.tokens.matrix <- as.matrix(train.tokens.dfm)
        View(train.tokens.matrix[1:20,1:100])
        
        # In dealing with text analytics,
        # the size of the data explodes very quickly
        # check out the sheer size of the columns below:
        dim(train.tokens.matrix)
        
        # Investigate the effects of stemming:
        colnames(train.tokens.matrix)[1:50]

# **********************************************************************************************************#
# (III): Text Analytics with R_Lecture 04: *Building Our First Model*
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/')

# From our dfm ('bag of words' model), setup a feature dataframe with Labels
train.tokens.df <- cbind(Label = train$Label , as.data.frame(train.tokens.dfm))

  # We're almost done with our pre-processing and setting up our final dataframe,
  # the last thing needed is fixing the column names of our dataframe
  names(train.tokens.df)[c(146,149,235)]
  
  # Cleaning-up the column names using 'make.names()'
  # It makes SYNTACTICALLY (may look absurd) valid names
  names(train.tokens.df) <- make.names(names(train.tokens.df))

# Now, our data is prepped, and hence we can train our first model
# Use caret to create stratified folds for 10-fold cross validation,
# repeated 3 times (i.e create 30 random stratified samples)
set.seed(48743)
cv.folds <- createMultiFolds(train$Label, k=10, times=3) 
# Note: 'createMultiFolds' is like a wrapper to 'createDataPartition',
# i.e, createDataPartition is a subset to createMultiFolds
cv.ctrl <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        index = cv.folds)
# trainControl: is the process, by which caret will execute the training,
# of the model. Note, we have also specified (index = cv.folds),
# hence caret needn't worry about the no.of folds our model would need.

# install.packages("doSNOW")
library(doSNOW)
# Time the code execution
start.time <- Sys.time()
# Create a cluster to work on 3 logical cores
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
# Using 'rpart' (a single decision tree algorithm), and not 'rf',
# which is a multi-descision tree algo, because, 'rpart' is relatively faster.
rpart.cv.1 <- train(Label ~ .,   #Label ~ . => all the frequencies of text words in train.tokens.df
                    data = train.tokens.df,
                    method = 'rpart',     # => is the algo to train our model
                    trControl = cv.ctrl,  # => trControl = cv.ctrl = trainControl(deduced above)
                    tuneLength = 7)       # => tells caret to use 7 different 
# Processing is done, stop cluster
stopCluster(cl)
# Total time of execution of the ML algo
total.time <- Sys.time() - start.time
total.time
# Check out our results
rpart.cv.1

#*********************************************************************************************************#
# (IV): Text Analytics with R_Lecture 05: *Calculating TF-IDF*
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/')

# TF(t,d) = frequency(t,d)/Summation(frequency(t,d))
# freq. of 'terms'/total no. of 'terms' in a document
# => Normalization
# IDF(t) = log10(N/Count(t))
# total no. of documents/no. of documents with that 'term' in it
# => Rationalization
# TF - IDF(t,d) = TF(t,d) * IDF(t)

# Step.1: Calculate TF,IDF,and, TF-IDF
  # Calculating TF
  term.frequency <- function(row) {
    row/sum(row)
  }
  
  # Calculating IDF
  inverse.doc.freq <- function(col) {
    corpus.size <- length(col)
    doc.count <- length(which(col>0))
    
    log10(corpus.size/doc.count)
  }
  
  # Calculating TF-IDF
  tf.idf <- function(tf,idf) {
    tf*idf
  }

# Step.2: Normalize all documents by applying TF
  train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
  # Note: Here, 1 is for 'rows' in TF, as 2 is for 'columns' in IDF.
  
  dim(train.tokens.df)  #Notice, that apply() has transposed the matrix
  View(train.tokens.df[1:20,1:40])

# Step.3: Calculate IDF vector, that we'll use for both (train data & test data)
  train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
  str(train.tokens.idf)

# Step.4: Calculate the mighty 'TF-IDF' for our training corpus
  train.tokens.tfidf <- apply(train.tokens.df, 2, tf.idf, train.tokens.idf)
  # Note: tf = train.tokens.df,
  # Since the matrix was transposed by apply(), hence we'd run it on 2,
  # which essentially is 1(row), because of transposition.
  # tf.idf = the function for apply()
  # idf = train.tokens.idf
  
  View(train.tokens.tfidf)
  # After performing 'tfidf' (Step.4), each document(text1, text2,...text n),
  # is now rationalized (before it was just normalized (each document was normalized,
  # based on it's length, so each doc could be compared on equal footing),
  # now individual terms('go','jurong','crazi'...) are rationalized in the corpus,
  # which says: look, those terms that appear more frequently in the documents,
  # are going to be less useful(hence less value of those terms), than the ones
  # which are appear less frequently(hence more value to those terms).
  # Because the terms that appear less frequent in the documents,
  # carry more predictive power.

# Step.5 : Transpose our mighty 'tfidf' back,
  # because right now it's in 'term-frequency-document-frequency-matrix',
  # where, term-frequencies are the rows, and documents are the columns.
  # So we'd need to flip it back to ready it to train our Machine Model
  train.tokens.tfidf <- t(train.tokens.tfidf)
  dim(train.tokens.tfidf)
  View(train.tokens.tfidf[1:20,1:20])

# Step.6: Check for Incomplete cases
  # Why?
  # Here's Why: when we invoke 'tfidf', we need to check for a particular
  # degenerative case.
  # What is that degen case?
  # Here's What it is: After we've done all our pre-preocessing (i.e removing of
  # all the stopwords, removing symbols, numbers,etc, after we've stemmed),
  # it's entirely possible that there's nothing left. We basically might have
  # removed eberything from the string.
  # Eg.: Imagine we had a string of emoticons(bunch of special chars/symobols).
  # After the pre-processing,there's a strong probability that
  # all the special chars/symbols may have been wiped off,
  # hence there'd be nothing left but the empty string.
  # So when we run 'tfidf' calculation on an empty string, we get error from R.
  # error like 'NaN'.
  # So to check if there's any Incomplete cases left or not, we'd do the following:
  incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
  train$Text[incomplete.cases]
  
  # Fixing these incomplete cases
  train.tokens.tfidf[incomplete.cases, ] <- rep(0, ncol(train.tokens.tfidf))
  
  # Verifying the same:
  train.tokens.tfidf[incomplete.cases]
  dim(train.tokens.tfidf)
  sum(which(!complete.cases(train.tokens.tfidf)))

# Step.7: Make a clean data frame using the same process as before
  train.tokens.tfidf.df <- cbind(Label = train$Label, as.data.frame(train.tokens.tfidf))
  names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))
  
  View(train.tokens.tfidf.df)

# Step.8: Training the model.
  # Invoke "doSNOW" 
  library(doSNOW)
  
  # Time the code execution
  start.time <- Sys.time()
  
  # Create a cluster to work on the 3 logical cores
  cl <- makeCluster(3, type='SOCK')
  registerDoSNOW(cl)
  
  rpart.cv.2 <- train(Label ~ .,
                      data = train.tokens.tfidf.df,
                      method = 'rpart',
                      trControl = cv.ctrl, #Note: cv.ctrl is of : 'Our first model'
                      tuneLength = 7)
  stopCluster(cl)
  
  total.time <- Sys.time() - start.time
  rpart.cv.2

#*********************************************************************************************************#
# (V): Text Analytics with R_Lecture 06: *N-grams*
getwd()
setwd('C:/Users/abhi/Downloads/Jobs/New Job/R/')

# Big files:
  #remove(train.tokens.matrix)
  #remove(train.tokens.tfidf)
  #remove(train.tokens.tfidf.df)
  #remove(train.tokens.df)
  #remove(train.tokens)
  #remove(train.tokens.dfm)
  #remove(train.tokens.idf)
  #remove(incomplete.cases)
  #remove(indices)
  #remove(inverse.doc.freq)
  #remove(tf.idf)
  #remove(term.frequency)
  #remove(cl)
  #remove(cv.ctrl)
  #remove(cv.folds)
  #remove(test)
  #remove(train)
  #remove(spam.raw)

# Adding bi-grams to our feature matrix
train.tokens <- tokens_ngrams(train.tokens, n=1:2)
train.tokens[[358]]

# Transform to dfm, and then a matrix
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.matrix <- as.matrix(train.tokens.dfm)
train.tokens.dfm

# Normalize all documents via TF
View(train.tokens.df)
train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)

# Calculate the IDF vector that we'll use for (train & test data)
train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)

# Calculate TF-IDF for our training corpus.
# Note: the resultant 'train.tokens.tfidf' is a 'document-term matrix'(row=doc, col=terms)
train.tokens.tfidf <- apply(train.tokens.df, 2, tf.idf, train.tokens.idf)

# Transpose the matrix, so that it becomes: 'term-document matrix'(row=term, col=doc)
train.tokens.tfidf <- t(train.tokens.tfidf)

# Fix incomplete cases
incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train.tokens.tfidf[incomplete.cases, ] <- rep(0, ncol(train.tokens.tfidf))

# Make a clean data frame
train.tokens.tfidf.df <- cbind(Label = train$Label,
                               as.data.frame(train.tokens.tfidf))
names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))

# Clean-up unused objects in memory
gc()
