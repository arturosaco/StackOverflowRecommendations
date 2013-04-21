library(Metrics)
#setwd("/Users/sergio/Programs/mahout/recommender/datasets")


getList <- function(question, data) {
	sub <- data[data$question==question, c(2, 3)]
	return(sub[order(sub$score, decreasing=TRUE), 1])
}

getScore <- function(question, data, data.ref) {
	predicted <- getList(question, data)
	if ( length(predicted) == 0 )
		return(0)
	actual <- getList(question, data.ref)
	mapk(length(predicted), actual[1:length(predicted)], predicted)
}


# Reading test file
test <- read.csv("data/Three_Months/matrix_test.csv", header=TRUE)

# Reading recommendations
rec <- read.csv("logs/recommendations.csv", header=TRUE)

# Computing Score
user.ids <- unique(test$question)
score <- sum(sapply(user.ids, function(x) getScore(x, rec, test))) 
print(score)


