# Grow trees on classified data
# Nora Schinkel & Johnno de Vos

####################### SPLIT AND LABEL #################################

# Calc impurity by Gini index
impurity <- function(vector) {
	# find p
	p <- sum(vector) / length(vector)
	# calc p*1-p (Gini index)
	imp <- p*(1-p)
	return(imp)
}

# Calc bestsplit
bestsplit <- function(x, y, minleaf, splitpoints) {
	i_t <- impurity(y)
	l <- length(y)

	best_red <- 0
	best_val <- 0

	for (val in splitpoints) {

		y_l <- y[x <= val]
		y_r <- y[x > val]

		# minleaf condition
		if (length(y_l) < minleaf || length(y_r) < minleaf) next

		pi_l <- length(y_l) / l
		pi_r <- length(y_r) / l

		imp_red <- i_t - ( pi_l * impurity(y_l) + pi_r * impurity(y_r) )

		if (imp_red > best_red){
			best_red <- imp_red
			best_val <- val
		}
	}

	return(c(best_val, best_red))
}

# Finds the best split over all features
tree.createsplit <- function(x, y, nmin, minleaf, nfeat){
	# number of features
	tot_feat <- length(x)

	# choose features with nfeat !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	# generate nfeat random numbers from tot_feat
	rand_feat <- c(1:nfeat)

	# calc splitpoints for these features
	best_split.feat <- 0
	best_split.imp_red <- 0
	best_split.val <- 0

	for (feat in rand_feat) {
		vec_x <- x[,feat]

		# calc splitpoints to check (no segment check)
		vec_x.ready <- sort(unique(vec_x))
		vec_x.ready_len <- length(vec_x.ready)
		vec_x.splitpoints <- (vec_x.ready[1:vec_x.ready_len-1]+vec_x.ready[2:vec_x.ready_len])/2

		vec_x.best_split <- bestsplit(vec_x, y, minleaf, vec_x.splitpoints)

		# if higher impurity reduction than best split, set this as best split
		if (vec_x.best_split[2] > best_split.imp_red){
			best_split.feat <- feat
			best_split.imp_red <- vec_x.best_split[2]
			best_split.val <- vec_x.best_split[1]
		}
	}
	
	return(c(best_split.feat, best_split.val))

}

#calculate the label
tree.label <- function(y, nodes){
	label <- 0
	labels <- y[nodes]
	percentage_ones <- sum(labels)/length(labels)
	if (percentage_ones > 0.5) {
		label <- 1
	}
	return(label)
}

####################### GROW AND BAG #################################

# grow a tree on a sample x, with labels y
# the tree is a list of c objects. each node n is spaced n*2and n*2+1 from its children
tree.grow <- function(x, y, nmin, minleaf, nfeat) {

	# root node
	feat_val <- tree.createsplit(x, y, nmin, minleaf, nfeat)

	# no split from root
	if(feat_val[1] == 0){
		return(data.frame("feat"=0, "val"=0, "label"=label(y,c(1:length(y)))))
	}

	col_split.col <- x[,feat_val[1]]
	col_split.val <- feat_val[2]

	# place rootnode
	place.old <- 1
	nodes <- c(1:nrow(x))
	tree <- data.frame("feat"=feat_val[1], "val"=col_split.val, "label"=-1)

	
	#  GO DOWN
	node_left <- which(col_split.col < col_split.val)
	tree <- tree.left_down(tree, node_left, place.old*2, x, y, nmin, minleaf, nfeat)
	
	node_right <- which(col_split.col > col_split.val)
	tree <- tree.left_down(tree, node_right, place.old*2+1, x, y, nmin, minleaf, nfeat)
	
	return(tree)

}

# grow down from given node, favouring left
tree.left_down <- function(tree, node_left, place.new, x, y, nmin, minleaf, nfeat){
	while (!node_left[1] == 0){

		# nmin constraint 
		if (length(node_left) > nmin){
			#new split
			new_y <- y[node_left]
			new_x <- x[node_left,]
			feat_val <- tree.createsplit(new_x, new_y, nmin, minleaf, nfeat)
			col_split.col <- x[,feat_val[1]]
			col_split.val <- feat_val[2]

			# if there is no good split available, create a leaf node
			if(col_split.val == 0){
				label <- tree.label(y, node_left)
				tree[place.new,] <- c(0, 0, label)
				node_left <- c(0)
				}
			# else place node in tree and set everything up for creating children
			else {
				tree[place.new,] <- c(feat_val[1], col_split.val, -1)
				new_node.left <- intersect(which(col_split.col < col_split.val), node_left)
				new_node.right <- intersect(which(col_split.col > col_split.val), node_left)
				tree <- tree.left_down(tree, new_node.right, place.new * 2 + 1, x, y, nmin, minleaf, nfeat)
				node_left <- new_node.left
				place.new <- place.new * 2

			}
		} else {
			# write leafnode
			label <- tree.label(y, node_left)
			tree[place.new,] <- c(0, 0, label)
			node_left <- c(0)
		}
	}
	return(tree)
}

# grow m trees over m bootstrap samples
tree.grow.bag <- function(x, y, nmin, minleaf, nfeat, m){
	returnlist <- list()
	#create m trees
	for(i in 1:m){
		x$y <- y
		training <- tree.bootstrap_sample(x, m)
		new_y <- training[,ncol(training)]
		training$y <- NULL
		returnlist[[i]] <- tree.grow(training, new_y, nmin, minleaf, nfeat)
	}
	return(returnlist)
}

# create a sample of size m from data, with replacement
tree.bootstrap_sample <- function(data, m){
	return(data[sample(1:nrow(data), m, replace=T),])
}


####################### CLASSIFY AND BAG #################################

tree.classify <- function(x, tree) {
  labeled <- vector()
  
  for(i in 1:nrow(x)){
    output <- tree.classify_rec(x[i,], tree, 1)
    labeled <- c(labeled, output)
  }
  return(labeled)
}

tree.classify_rec <- function(x, tree, n){
  
  node <- tree[n,]
  feature <- node$feat
  value <- node$val
  
  if(feature == 0 && value == 0)
    return(node$label)
  
  if(x[feature] < value)
    tree.classify_rec(x, tree, 2*n)
  else if(x[feature] >= value)
    tree.classify_rec(x, tree, 2*n+1)
  
}

# classify bagged trees by choosing majority vote after m trees
tree.classify.bag <- function(x, trees){
	prep <- seq(0,0,length.out=nrow(x))
	#keep track of how many classified 1's and 0's
	prediction <- data.frame("0"=prep, "1"=prep)

	# fill prediction frame
	for(tree in trees){
		cl <- tree.classify(x, tree)
		prediction <- tree.classify.bag.count(prediction, cl)
	}
	
	# take majority vote
	final_prediction <- prep
	final_prediction[which(prediction[,1] < prediction[,2])] <- 1
	return(final_prediction)
}

# sums the newly classified labels to the prediction dataframe
tree.classify.bag.count <- function(prediction, new_data){
	for(i in 1:length(new_data)){
		if(new_data[i] == 0){
			prediction[i,1] <- prediction[i,1] + 1
		}
		else{
			prediction[i,2] <- prediction[i,2] + 1
		}
	}
	return(prediction)
}

####################### TESTS #################################

# credit.dat <- read.csv("_data/credit.txt")
# credit.x <- credit.dat[1:5]
# credit.y <- credit.dat[,6]

# trees <- tree.grow.bag(credit.x, credit.y, 1, 1, 5, 5)
# print(tree.classify.bag(credit.x, trees))

# cut unwanted values from the data
preprocess <- function(csvfile){
	credit.dat <- read.csv(csvfile, header=TRUE, sep=";")
	credit.dat$post <- as.numeric(credit.dat$post > 0)
	# package_names <- credit.dat$packagename
	credit.dat$plugin <- NULL
	credit.dat$packagename <- NULL
	# credit.dat$post <- NULL
	credit.dat <- credit.dat[1:42]
	return(credit.dat)

}

# computes accuracy, recall and precision
measures <- function(returned, real){
	returned.positives <- sum(returned)
	real.positives <- sum(real)
	returned.true_positives <- length(which(real+returned == 2))
	measure.recall <- returned.true_positives/real.positives
	measure.precision <- returned.true_positives/returned.positives

	# ACCURACY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	print(c(measure.recall, measure.precision))
}

# set training set, test set and variables

train.x <- preprocess("_data/eclipse-metrics-packages-2.0.csv")
train.y <- train.x$post
train.x$post <- NULL

test.x <- preprocess("_data/eclipse-metrics-packages-3.0.csv")
test.y <- test.x$post
test.x$post <- NULL

nmin <- 15
minleaf <- 5
nfeat <- 41
m <- 10

#### normal tree #####

tree.normal <- tree.grow(train.x, train.y, nmin, minleaf, nfeat)
classify.normal <- tree.classify(test.x, tree.normal)

measures.normal <- measures(classify.normal, test.y)

#### bagged tree #####

tree.bagged <- tree.grow.bag(train.x, train.y, nmin, minleaf, nfeat, m)
classify.bagged <- tree.classify.bag(test.x, tree.bagged)

measures.bagged <- measures(classify.bagged, test.y)

#### forest #####

# TO DO
