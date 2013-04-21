library(XML)
library(Metrics)

# convert HTML to plain text
convert_html_to_text <- function(html) {
	doc <- htmlParse(html, asText = TRUE)
	text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
	return(paste(text, collapse = " "))
}

writeFileQuestion <- function(q) {
	path <- paste("texts/", q$question_id, sep="")
	dir.create(path, showWarnings=FALSE)
	file.conn <- file(paste(path, "/", q$question_id, "_q.txt", sep=""))
	question.text <- convert_html_to_text(q$question_body)
	writeLines(question.text, file.conn)
	close(file.conn)
	file.conn <- file(paste(path, "/", q$question_id, "_a.txt", sep=""))
	writeLines(paste(question.text,convert_html_to_text(q$answer_body),"\n\n"), file.conn)
	close(file.conn)
}

data.even <- read.csv("data/even.csv", header=TRUE)
data.odd <- read.csv("data/odd.csv", header=TRUE)
data.odd <- unique(data.odd)

data <- unique(rbind(data.even, data.odd))
data <- data[data$is_accepted != "is_accepted", ]
data$score <- as.numeric(as.character(data$score))
data$answer_owner <- as.numeric(as.character(data$answer_owner))

data <- data[!is.na(data$answer_owner),]
data$is_accepted <- as.character(data$is_accepted)
# d_ply(data, "question_id", function(sub){
#   if("True" %in% sub$is_accepted){
#     out <- sub[sub$is_accepted == "True", ]  
#   }else{
#     out <- sub[order(sub$score, decreasing = TRUE), ][1, ]
#   }
#   writeFileQuestion(out)
#   }, .progress = "text")

# for ( i in 1:nrow(data) ) {
# 	if ( as.character(data$is_accepted[i]) == "True" ) {
# 		print(paste(i,": id=",as.character(data$question_id[i]), sep=""))
# 		writeFileQuestion(data[i,])
# 	}
# }

data$score.1 <- -1000
data[data$is_accepted == "True", "score.1"] <- 
  data[data$is_accepted == "True", "score"] + 2
data[data$is_accepted == "False", "score.1"] <- 
  data[data$is_accepted == "False", "score"] + 1

matrix <- data[,c("answer_owner","question_id","score.1")]
names(matrix) <- c("userID", "itemID", "preference")
matrix$userID <- as.numeric(matrix$userID)
matrix$itemID <- as.numeric(as.character(matrix$itemID))
matrix$preference <- as.numeric(matrix$preference)
matrix <- matrix[!is.na(matrix$userID)&!is.na(matrix$itemID)&!is.na(matrix$preference),]

matrix.1 <- ddply(matrix, c("userID", "itemID"), summarise,
  preference = sum(preference),
 .progress = "text")

matrix.1 <- arrange(matrix.1, itemID)

set.seed(104742)
itemID.test <- sample(unique(matrix.1$itemID), 
  round(length(unique(matrix.1$itemID)) * .2))

matrix.sub <- matrix.1[matrix.1$itemID %in% itemID.test, ]

userID.test <- sample(unique(matrix.1$userID), 
  round(length(unique(matrix.1$userID)) * .1))

matrix.test <- matrix.sub[matrix.sub$userID %in% userID.test, ]

prop.table(table(matrix.1$userID %in% userID.test &
   matrix.1$itemID %in% itemID.test))

write.table(itemID.test, file = "data/itemID_test.txt")
write.table(userID.test, file = "data/userID_test.txt")

write.table(setdiff(matrix.1$itemID, itemID.test), file = "data/itemID_train.txt")
write.table(setdiff(matrix.1$userID, userID.test), file = "data/userID_train.txt")

write.table(matrix.1[matrix.1$itemID %in% itemID.test & 
  matrix.1$userID %in% userID.test, ],
 file = "data/matrix_test.txt")
write.table(matrix.1[!(matrix.1$itemID %in% itemID.test & 
  matrix.1$userID %in% userID.test), ],
 file = "data/matrix_train.txt")


random <- runif(nrow(matrix))
p <- 1/100
matrix.train <- matrix[random>p,]
matrix.test <- matrix[random<=p,]
write.table(matrix.train, "datasets/matrix_train.csv", row.names=FALSE,  col.names=FALSE, sep=",")
write.table(matrix.test, "datasets/matrix_test.csv", row.names=FALSE,  col.names=FALSE, sep=",")

# -----> Java program execution
#system("mvn compile")
#system("java -jar target/recommender-1.0-SNAPSHOT.jar")

# Load recommendations
rec <- read.csv("datasets/recommendations.csv", header=FALSE)
ids <- unique(matrix.test$userID)
score <- 
	sum(
		sapply(ids[1:1000],
        		  function(x) {
          			actual <- matrix.test[matrix.test[1]==x,2]
            		predicted <- rec[rec[1]==x,2]
            		if ( length(predicted) != 0 ) {
            			return(mapk(length(predicted), list(actual), list(predicted)))
            		} else
            			return(0)
          		} )
	)/length(ids)
print(score)