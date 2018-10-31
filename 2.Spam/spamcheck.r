library(tm)
library(SnowballC)
library(entropy)

# library(caret)
# setwd("C:/Users/Johnno/Documents/Data Mining/DataMining/2.Spam ")



reviews.neg <- VCorpus(DirSource("train/neg", encoding="UTF-8"))
reviews.pos <- VCorpus(DirSource("train/pos", encoding="UTF-8"))
reviews.test_neg <- VCorpus(DirSource("test/neg", encoding="UTF-8"))
reviews.test_pos <- VCorpus(DirSource("test/pos", encoding="UTF-8"))
# Join negative and positive reviews into a single Corpus
reviews.all <- c(reviews.neg, reviews.pos)
reviews.test_all <- c(reviews.test_neg, reviews.test_pos)
# create label vector (0=negative, 1=positive)
labels <- c(rep(0,320),rep(1,320))
labels_test <- c(rep(0,80),rep(1,80))


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


# returns list with indexes of folds
get_folds <- function(){
	range <- c(1:640)
	folds.no <- 4
	folds.list <- list()
	while(folds.no > 0) {
		s <- sample(range, 160, replace=F)
		folds.list[[folds.no]] <- s
		range <- range[-s]
		folds.no <- folds.no -1
	}
	return (folds.list)
}

measures <- function(table){
	accuracy <- (table[1,1]+table[2,2])/160
	recall <- table[2,2]/(table[1,2]+table[2,2])
	precision <- table[2,2]/(table[2,1]+table[2,2])
	f_measure <- (2*precision*recall)/(precision+recall)
	return(list(accuracy=accuracy, recall=recall, precision=precision,f_measure=f_measure))
}

######################### NAIVE BAYES #############################


bayes.topfeat <- function(ntopfeat, train.dtm, train.labels) {
	# compute mutual information of each term with class label
	train.mi <- apply(as.matrix(train.dtm),2,
	    function(x,y){mi.plugin(table(x,y)/length(y))},train.labels)
	# sort the indices from high to low mutual information and get ntopfeat best
	train.mi.order <- order(train.mi,decreasing=T)
	return (train.mi.order[1:ntopfeat])

}


bayes.train_hyper <- function(topfrom, topto, reviews, no_runs) {
	ntopfeat <- topfrom
	range <- c(1:640)
	best.topfeat <- ntopfeat
	best.accuracy <- 0
	# test for all values of ntopfeat
	while(ntopfeat <= topto) {
		print(ntopfeat)
		accuracy.total <- 0

		# measure for no_runs amount of runs and take average
		for(no in c(1:no_runs)){
			folds.all <- get_folds()
			folds.val <- 1
			accuracy.run <- 0
			while (folds.val <= 4){
				# extract indices for current fold
				test.indices <- folds.all[[folds.val]]
				train.indices <- range[-test.indices]

				# create dtms
				train.dtm <- DocumentTermMatrix(reviews.all[train.indices])
				train.dtm <- removeSparseTerms(train.dtm, 0.95)

				test.dtm <- DocumentTermMatrix(reviews.all[test.indices],
		               list(dictionary=dimnames(train.dtm)[[2]]))

				# get top features
				train.labels <- labels[train.indices]
				train.top_feat <- bayes.topfeat(ntopfeat, train.dtm, train.labels)

				# train and test for no_runs
				
				model.trained <- train.mnb(as.matrix(train.dtm)[,train.top_feat], train.labels)

				# predict on the test set
				model.predicted <- predict.mnb(model.trained, as.matrix(test.dtm)[,train.top_feat])
				model.table <- table(model.predicted,labels[test.indices])

				# add accuary and start new
				accuracy.run <- accuracy.run + (model.table[1,1]+model.table[2,2])/160
				folds.val <- folds.val + 1 
			}
			# take mean over folds
			accuracy.total <- accuracy.total + accuracy.run/4	
		}

		# update if better score
		accuracy.total <- accuracy.total/no_runs
		if (accuracy.total > best.accuracy){
			best.accuracy <- accuracy.total
			best.topfeat <- ntopfeat
		}

		ntopfeat <- ntopfeat + 1
	}
	print(best.accuracy)
	print(best.topfeat)
	return(best.topfeat)
}

bayes.train_test <- function(train, train_labels, test, test_labels, ntopfeat, no_runs){
	accuracy <- 0
	precision <- 0
	recall <- 0
	for(no in c(1:no_runs)){
		train.dtm <- DocumentTermMatrix(train)
		train.dtm <- removeSparseTerms(train.dtm, 0.95)
		test.dtm <- DocumentTermMatrix(test, list(dictionary=dimnames(train.dtm)[[2]]))
		train.top_feat <- bayes.topfeat(ntopfeat, train.dtm, train_labels)
		model.trained <- train.mnb(as.matrix(train.dtm)[,train.top_feat], train_labels)
		model.predicted <- predict.mnb(model.trained, as.matrix(test.dtm)[,train.top_feat])
		model.table <- table(model.predicted,test_labels)
		measures <- measures(model.table)
		accuracy <- accuracy + measures$accuracy
		precision <- precision + measures$precision
		recall <- recall + measures$recall
	}
	accuracy <- accuracy/no_runs
	precision <- precision/no_runs
	recall <- recall/no_runs
	f_measure <- (2*precision*recall)/(precision+recall)
	return(list(accuracy=accuracy, recall=recall, precision=precision,f_measure=f_measure))
}

# from slides
train.mnb <- function (dtm,labels) {
	call <- match.call()
	V <- ncol(dtm)
	N <- nrow(dtm)
	prior <- table(labels)/N
	labelnames <- names(prior)
	nclass <- length(prior)
	cond.probs <- matrix(nrow=V,ncol=nclass)
	dimnames(cond.probs)[[1]] <- dimnames(dtm)[[2]]
	dimnames(cond.probs)[[2]] <- labelnames
	index <- list(length=nclass)
	for(j in 1:nclass){
	index[[j]] <- c(1:N)[labels == labelnames[j]] }
	for(i in 1:V){
		for(j in 1:nclass){
			cond.probs[i,j] <- (sum(dtm[index[[j]],i])+1)/(sum(dtm[index[[j]],])+V) }
	} 
	return(list(call=call,prior=prior,cond.probs=cond.probs))
}

# from slides
predict.mnb <- function (model,dtm) {
	classlabels <- dimnames(model$cond.probs)[[2]]
	logprobs <- dtm %*% log(model$cond.probs)
	N <- nrow(dtm)
	nclass <- ncol(model$cond.probs)
	logprobs <- logprobs+matrix(log(model$prior),nrow=N,ncol=nclass,byrow=T)
	return(classlabels[max.col(logprobs)])
}

###########
# HYPER PARAM TEST ##
from <- 100
to <- 102
runs <- 10
##########

bayes.hyper_param <- bayes.train_hyper(from,to,reviews.all,runs)
bayes.train_test(reviews.all, labels, reviews.test_all, labels_test, bayes.hyper_param, runs)

#########################################################################
# print(train.dtm)
# print(inspect(train.dtm))


