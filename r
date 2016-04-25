## PART I

### Import data from Twitter ###
library(plyr)
library(stringr)

Kasich <- searchTwitter("kasich", n=1200, lang= "en", since=NULL, until=NULL, retryOnRateLimit=20)
kasich <- strip_retweets(Kasich, strip_manual = TRUE, strip_mt = TRUE)

# Extract tweet text
kasichText <- laply(kasich,function(t) t$getText() )
kasichText <- iconv(kasichText, "latin1", "ASCII", sub="")

# Import sentiment libraries
positive <- scan("~/Dropbox/AccelerateAnalytics/Datasets/opinion-lexicon-English/positive-words.txt", what = "character", comment.char = ";")
negative <- scan("~/Dropbox/AccelerateAnalytics/Datasets/opinion-lexicon-English/negative-words.txt", what = "character", comment.char = ";")

# Add a few more industry specific positive, negative words
pos <- c(positive, "beat")
neg <- c(negative, "wtf", "patsy")




### PART II:  SENTIMENT SCORING  ####
### Twitter Sentiment Score ###

### Scoring :  Thanks to https://jeffreybreen.wordpress.com/tag/sentiment-analysis/

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}




### PART III: SENTIMENT ANALYSIS ###

### Analysis ###
analysis <- score.sentiment(kasichText, pos, neg, .progress='text')
analysis

### write to CSV file ###
write.csv(analysis, "TwitterDump.csv") 


# Evaluate sentiment
analysis$Negative = as.factor(analysis$score <= 1)
table(analysis$Negative)


#### Create Corpus ####
library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(analysis$text))

corpus <- tm_map(corpus, content_transformer(tolower), lazy=TRUE)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument, lazy=TRUE)
corpus <- tm_map(corpus, removePunctuation, lazy=TRUE)
corpus <- tm_map(corpus, removeWords, c("apple", "cruz", "kasich", "kaisch", "trump", stopwords("english")), lazy=TRUE)
corpus <- tm_map(corpus, stemDocument, lazy=TRUE)


dtm <- DocumentTermMatrix(corpus)
print(dtm)

findFreqTerms(dtm, lowfreq = 20)
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = TRUE)
head(v,10)

# Remove sparse terms
sparseterms <- removeSparseTerms(dtm, .99)
sparseterms

#Convert to a data frame
tweets <- as.data.frame(as.matrix(sparseterms))
colnames(tweets)  <-make.names(colnames(tweets))

# Add back dependent variable
tweets$Sentiment <- analysis$Negative






###  PART IV:  Sentiment Modeling  ###

### Break into train and test segments
library(caTools)
set.seed(99)

split <- sample.split(tweets$Sentiment, SplitRatio = 0.6)

train <- subset(tweets, split==TRUE)
test <- subset(tweets, split==FALSE)




# Build a random forest model
library(rpart)
library(rpart.plot)

tweetTree <- rpart(Sentiment ~ ., data=train, method = "class")
prp(tweetTree)

# Evaluate the performance of the model
predictTree <- predict(tweetTree, newdata=test, type="class")
table(test$Sentiment, predictCART)

# Compute model accuracy
checkAccuracy <- table(test$Sentiment, predictTree)
sum(diag(checkAccuracy))/nrow(test)

# Baseline accuracy 
sum(checkAccuracy[1, ])/nrow(test)



# Random forest model

library(randomForest)
set.seed(NULL)
set.seed(123)

tweetRF <- randomForest(Sentiment ~ ., data=train)

# Make predictions:
predictRF <- predict(tweetRF, newdata=test)
table(test$Sentiment, predictRF)

# Accuracy:
checkAccuracyRF <- table(test$Sentiment, predictRF)
sum(diag(checkAccuracyRF))/nrow(test)

