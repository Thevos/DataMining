credit.dat <- read.csv("_data/credit.txt")
credit.x <- credit.dat[1:5]
credit.y <- credit.dat[,6]

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

	# choose features with nfeat
	# generate nfeat random numbers from tot_feat
	rand_feat <- c(1,2,3,4,5)

	# calc splitpoints for these features
	best_split.feat <- 0
	best_split.imp_red <- 0
	best_split.val <- 0

	for (feat in rand_feat) {
		vec_x <- x[,feat]
		# print(vec_x)

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
				tree[[place.new]] <- c(0, 0, label)
				node_left <- c(0)
				}
			# else place node in tree and set everything up for creating children
			else {
				tree[[place.new]] <- c(feat_val[1], col_split.val, node_left)
				new_node.left <- intersect(which(col_split.col < col_split.val), node_left)
				new_node.right <- intersect(which(col_split.col > col_split.val), node_left)
				tree <- tree.left_down(tree, new_node.right, place.new * 2 + 1, x, y, nmin, minleaf, nfeat)
				node_left <- new_node.left
				place.new <- place.new * 2

			}
		} else {
			# write leafnode
			label <- tree.label(y, node_left)
			tree[[place.new]] <- c(0, 0, label)
			node_left <- c(0)
		}
	}
	return(tree)
}

# remove all extra items from the nodes
tree.cut <- function(tree){
	index <- 0
	for (node in tree){
		index <- index + 1
		if (!is.null(node)){
			if (!node[1] == 0){
				tree[[index]] <- node[1:2]
			}
		}
	}
	return(tree)
}

# grow a tree on a sample x, with labels y
# the tree is a list of c objects. each node n is spaced n*2and n*2+1 from its children
tree.grow <- function(x, y, nmin, minleaf, nfeat) {

	# setup tree
	tree <- list()

	# root node
	feat_val <- tree.createsplit(x, y, nmin, minleaf, nfeat)

	col_split.col <- x[,feat_val[1]]
	col_split.val <- feat_val[2]

	# place rootnode
	place.old <- 1
	nodes <- c(1:nrow(x))
	node_root <- c(feat_val[1], col_split.val, nodes)
	tree[[1]] <- node_root

	
	#  GO DOWN
	node_left <- which(col_split.col < col_split.val)
	tree <- tree.left_down(tree, node_left, place.old*2, x, y, nmin, minleaf, nfeat)
	
	node_right <- which(col_split.col > col_split.val)
	tree <- tree.left_down(tree, node_right, place.old*2+1, x, y, nmin, minleaf, nfeat)

	# take care of extra values
	tree <- tree.cut(tree)
	print(tree)

}

tree.grow(credit.x, credit.y, 1, 1, 5)










