library(tm)
library(SnowballC)
library(caret)
setwd("C:/Users/Johnno/Documents/Data Mining/DataMining/2.Spam ")
#txt.neg <- system.file("C:/Users/Johnno/Documents/Data Mining/DataMining/2.Spam/train/neg", "txt", package = "tm")
#txt.pos <- system.file("C:/Users/Johnno/Documents/Data Mining/DataMining/2.Spam/train/pos", "txt", package = "tm")
#print(txt.neg)
reviews.neg <- VCorpus(DirSource("train/neg", encoding="UTF-8"))
reviews.pos <- VCorpus(DirSource("train/pos", encoding="UTF-8"))
# Join negative and positive reviews into a single Corpus
reviews.all <- c(reviews.neg, reviews.pos)
# create label vector (0=negative, 1=positive)
labels <- c(rep(0,320),rep(1,320))

#inspect(reviews.all)

# Remove punctuation marks (comma's, etc.)
reviews.all <- tm_map(reviews.all,removePunctuation)
# Make all letters lower case
reviews.all <- tm_map(reviews.all, content_transformer(tolower))
# Remove stopwords
reviews.all <- tm_map(reviews.all, removeWords, stopwords("english"))
# Remove numbers
reviews.all <- tm_map(reviews.all,removeNumbers)
# Remove excess whitespace
reviews.all <- tm_map(reviews.all,stripWhitespace)
# Stem the reviews
reviews.all <- tm_map(reviews.all, stemDocument, language="english")

folds <- createFolds(reviews.all, k = 5)


# draw training sample (stratified)
# draw 8000 negative reviews at random
index.neg <- sample(320,8000)
# draw 8000 positive reviews at random
index.pos <- 320+sample(320,8000)
index.train <- c(index.neg,index.pos)
# create document-term matrix from training corpus
train.dtm <- DocumentTermMatrix(reviews.all[index.train])
#dim(train.dtm)

# remove terms that occur in less than 5% of the documents
# (so-called sparse terms)
train.dtm <- removeSparseTerms(train.dtm,0.95)

reviews.mnb <- train.mnb(as.matrix(train.dtm),labels[index.train])
# create document term matrix for test set
test.dtm <- DocumentTermMatrix(reviews.all[-index.train], list(dictionary=dimnames(train.dtm)[[2]]))

reviews.mnb.pred <- predict.mnb(reviews.mnb,as.matrix(test.dtm))
table(reviews.mnb.pred,labels[-index.train])
