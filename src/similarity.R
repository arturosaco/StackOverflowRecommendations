library(lsa)
#setwd("/Users/sergio/Programs/mahout/recommender/datasets")
NUM_REC <- 50

load("features.Rdata") # train.features, users, test.questions
load("newquestions.Rdata") # new.features

test.questions <- read.csv("itemID_test.txt", header=TRUE, sep=" ")
ntopics <- 100
nquestions <- nrow(test.questions)
new.features <- matrix(rnorm(nquestions*ntopics), nrow=nquestions, ncol=ntopics)


ranking <- function(vector, matrix, users) {
	scores <- apply(matrix, 1, function(x) cosine(vector, x))
	ind <- order(scores, decreasing=TRUE)
	return(sapply(ind[1:NUM_REC], function(x) users[x,1]))
}

results <- function(question, list) {
	matrix <- data.frame()
	for (i in 1:length(list)) {
		matrix <- rbind(matrix, c(question, list[i], length(list)-i+1))
	}
	names(matrix) <- c("question", "user", "score")
	return(matrix)
}


rec <- data.frame()
for (i in 1:nrow(test.questions)) {
	print(i)
	rec <- rbind(rec, results(test.questions[i,1], ranking(new.features[i,], train.features, users)) )
}