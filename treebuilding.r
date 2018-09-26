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

		# print(imp_red)

		if (imp_red > best_red){
			best_red <- imp_red
			best_val <- val
		}
	}

	# print(best_red)
	return(c(best_val, best_red))
}

income.sorted <- sort(unique(credit.dat[,4]))
income.splitpoints <- (income.sorted[1:7]+income.sorted[2:8])/2

# print(length(x))
# bestsplit(credit.dat[,4],credit.dat[,6], 2, income.splitpoints)

tree.grow <- function(x, y, nmin, minleaf, nfeat) {

	# number of features
	tot_feat <- length(x)

	# 1 splitloop:

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

		# print(vec_x.splitpoints)
		vec_x.best_split <- bestsplit(vec_x, y, minleaf, vec_x.splitpoints)
		# print(vec_x.best_split)

		# if higher impurity reduction than best split, set this as best split
		if (vec_x.best_split[2] > best_split.imp_red){
			best_split.feat <- feat
			best_split.imp_red <- vec_x.best_split[2]
			best_split.val <- vec_x.best_split[1]
		}
	}

	print(best_split.feat)
	print(best_split.imp_red)
	print(best_split.val)

	# next: verzamel nieuwe punten en ga door. Als geen split gevonden is (bijv. best_split.feat = 0) dan dus een leaf aanmaken
	# check for nmin constraint
}

tree.grow(credit.x, credit.y, 5, 2, 5)










