library(tm)
txt <- system.file("data/fold1", "txt", package = "tm")
reviews.neg <- Corpus(DirSource(txt, encoding="UTF-8"))