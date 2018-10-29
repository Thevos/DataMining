library(tm)
txt.neg <- system.file("train/neg", "txt", package = "tm")
txt.pos <- system.file("train/pos", "txt", package = "tm")
reviews.neg <- Corpus(DirSource(txt.neg,
                        encoding="UTF-8"))
> reviews.pos <- Corpus(DirSource(txt.pos,
                        encoding="UTF-8"))
# Join negative and positive reviews into a single Corpus
> reviews.all <- c(reviews.neg,reviews.pos)
# create label vector (0=negative, 1=positive)
> labels <- c(rep(0,12500),rep(1,12500))
> reviews.all