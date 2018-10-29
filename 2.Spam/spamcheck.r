library(tm)
library(SnowballC)
setwd("C:/Users/Johnno/Documents/Data Mining/DataMining/2.Spam ")
#txt.neg <- system.file("C:/Users/Johnno/Documents/Data Mining/DataMining/2.Spam/train/neg", "txt", package = "tm")
#txt.pos <- system.file("C:/Users/Johnno/Documents/Data Mining/DataMining/2.Spam/train/pos", "txt", package = "tm")
#print(txt.neg)
reviews.neg <- VCorpus(DirSource("train/neg", encoding="UTF-8"))
reviews.pos <- VCorpus(DirSource("train/pos", encoding="UTF-8"))
# Join negative and positive reviews into a single Corpus
reviews.all <- c(reviews.neg, reviews.pos)
# create label vector (0=negative, 1=positive)
labels <- c(rep(0,12500),rep(1,12500))

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
reviews.all <- tm_map(reviews.all, stemDocument, language="english")
