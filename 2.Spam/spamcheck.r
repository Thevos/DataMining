library(tm)
library(SnowballC)
library(entropy)
library(glmnet)
library(methods)
library(RWeka)


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


############################### EXTRA FEATURES ###########################

# TF-IDF
# from slides
tf_idf.train <- function(train){
	train.dtm <- DocumentTermMatrix(train, control=list(weighting=weightTfIdf))
	train.dtm <- removeSparseTerms(train.dtm,0.95)
	return (as.matrix(train.dtm))
}
# from slides
tf_idf.test <- function(train, test){
	train.dtm <- DocumentTermMatrix(train)
	train.dtm <- removeSparseTerms(train.dtm, 0.95)
	train.matrix <- as.matrix(train.dtm)
	col <- ncol(train.matrix)
	row <- nrow(train.matrix)
	test.dtm <- DocumentTermMatrix(reviews.test_all, list(dictionary=dimnames(train.matrix)[[2]]))
	train.matrix<- matrix(as.numeric(train.matrix > 0),nrow=row,ncol=col)
	train.idf <- apply(train.matrix,2,sum)
	train.idf <- log2(16000/train.idf)
	test.matrix <- as.matrix(test.dtm)
	for(i in 1:col){test.matrix[,i] <- test.matrix[,i]*train.idf[i]}
	return (test.matrix)
}

# BIGRAMS
# returns train matrix (from slides)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
add_bigrams <- function(train){
	train.dtm2 <- DocumentTermMatrix(train, control = list(tokenize = BigramTokenizer))
	train.dtm2 <- removeSparseTerms(train.dtm2,0.99)
	train.dtm <- DocumentTermMatrix(train)
	train.dtm <- removeSparseTerms(train.dtm, 0.95)
	train.dat <- cbind(as.matrix(train.dtm),as.matrix(train.dtm2))
	return(train.dat)
}

############################### HELPER FUNCTIONS #######################

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

# returns train and test matrices based upon chosen method and train and test data
get_matrices <-function(train, test, method){
	# get matrices
	if (method == "weights"){
		train.matrix <- tf_idf.train(train)
		test.matrix <- tf_idf.test(train, test)
	} else {
		if(method=="normal"){
			train.dtm <- DocumentTermMatrix(train)
			train.dtm <- removeSparseTerms(train.dtm, 0.95)
			train.matrix <- as.matrix(train.dtm)
		} else if (method == "bigrams"){
			train.matrix <- add_bigrams(train)
		}
		test.dtm <- DocumentTermMatrix(test, list(dictionary=dimnames(train.matrix)[[2]]))
		test.matrix <- as.matrix(test.dtm)
	}
	return(list(test.matrix=test.matrix, train.matrix=train.matrix))
}

############################## NAIVE BAYES #############################


bayes.topfeat <- function(ntopfeat, train.matrix, train.labels) {
	# compute mutual information of each term with class label
	train.mi <- apply(train.matrix,2,
	    function(x,y){mi.plugin(table(x,y)/length(y))},train.labels)
	# sort the indices from high to low mutual information and get ntopfeat best
	train.mi.order <- order(train.mi,decreasing=T)
	return (train.mi.order[1:ntopfeat])

}


bayes.train_hyper <- function(topfrom, topto, reviews, no_runs, method) {
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

				# get matrices
				matrices <- get_matrices(reviews[train.indices], reviews[test.indices], method)
				train.matrix <- matrices$train.matrix
				test.matrix <- matrices$test.matrix

				# get top features
				train.labels <- labels[train.indices]
				train.top_feat <- bayes.topfeat(ntopfeat, train.matrix, train.labels)

				# train and predict on the test set
				model.trained <- train.mnb(train.matrix[,train.top_feat], train.labels)
				model.predicted <- predict.mnb(model.trained, test.matrix[,train.top_feat])
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

# main bayes function
bayes.start <- function(train, train_labels, test, test_labels, ntopfeat, no_runs, method){
	accuracy <- 0
	precision <- 0
	recall <- 0

	matrices <- get_matrices(train, test, method)
	train.matrix <- matrices$train.matrix
	test.matrix <- matrices$test.matrix

	# get top feats
	train.top_feat <- bayes.topfeat(ntopfeat, train.matrix, train_labels)

	# train&test for no_runs
	l_tot <- bayes.train_test(train.matrix[,train.top_feat], train_labels, test.matrix[,train.top_feat], test_labels)
	for(no in 1:(no_runs-1)){
		l <- bayes.train_test(train.matrix[,train.top_feat], train_labels, test.matrix[,train.top_feat], test_labels)
		l_tot <- mapply("+", l, l_tot)
	}
	return(l_tot/no_runs)
}

# returns measures of one train/test run
bayes.train_test <- function(train.m, train_labels, test.m, test_labels){
	model.trained <- train.mnb(train.m, train_labels)
	model.predicted <- predict.mnb(model.trained, test.m)
	model.table <- table(model.predicted,test_labels)
	return(measures(model.table))
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

######################## LOG REG ########################################


logreg.train_test <- function(train.matrix, labels, test.matrix, labels_test){
	# print(test.matrix)
	reviews.glmnet <- cv.glmnet(train.matrix,labels, family="binomial",type.measure="class")
	reviews.logreg.pred <- predict(reviews.glmnet, newx=test.matrix,s="lambda.1se",type="class")
	return(measures(table(reviews.logreg.pred,labels_test)))

}

logreg.start <- function(train, labels, test, labels_test, method, no_runs){
	matrices <- get_matrices(train, test, method) 
	l_tot <- logreg.train_test(matrices$train.matrix, labels, matrices$test.matrix, labels_test)
	for (i in 1:(no_runs-1)){
		l <- logreg.train_test(matrices$train.matrix, labels, matrices$test.matrix, labels_test)
		l_tot <- mapply("+", l, l_tot)
	}
	return(l_tot/no_runs)
}

############################ TESTS #############################################

########### HYPER PARAM BAYES ##
from <- 100
to <- 101
runs <- 10
method <- "normal"

# bayes.hyper_param <- bayes.train_hyper(from,to,reviews.all,runs, method=method)
# bayes.start(reviews.all, labels, reviews.test_all, labels_test, bayes.hyper_param, runs, method=method)

########## LOGREG ##

logreg.start(reviews.all, labels, reviews.test_all, labels_test, method="normal", runs)
