#!/usr/bin/env Rscript

recs.path <- "data/OneMonthOfThree/collaborativeFiltering/"

score.matrix.path <- paste(recs.path, "../matrix.csv", sep="/")
train.questions.path <- paste(recs.path, "../itemID_train.txt", sep="/")
test.questions.path <- paste(recs.path, "../itemID_test.txt", sep="/")
train.users.path <- paste(recs.path, "../userID_train.txt", sep="/")
test.users.path <- paste(recs.path, "../userID_test.txt", sep="/")

score <- read.csv(file = score.matrix.path)
train.questions <- read.table(file = train.questions.path)[,1]
test.questions <- read.table(file = test.questions.path)[,1]
train.users <- read.table(file = train.users.path)[,1]
test.users <- read.table(file = test.users.path)[,1]

score.train <- score[score$question_id %in% train.questions, ]
score.test <- score[score$question_id %in% test.questions, ]
score.CF.test <- score.test[score.test$user_id %in% test.users, ]

train.num.qs <- tapply(score.train$question_id, score.train$user_id, length)

score.q.test <- score.test[score.test$user_id %in% 
  names(train.num.qs)[train.num.qs >= 4],]
score.CF.test.sub <- score.CF.test[score.CF.test$user_id %in%
 names(train.num.qs)[train.num.qs >= 4],]
files <- dir(recs.path)
files <- paste(recs.path, files, sep = "")

library(plyr)
library(reshape2)
library(ggplot2)


recs <- as.matrix(read.csv(file = file.x))
recs <- recs[order(recs[,"question_id"]), ]
score <- ddply( score.CF.test.sub, "question_id",
  function(sub) {
    user.recs <- recs[recs[,"question_id"] == unique(sub$question_id),
     "user_id"]
    if(length(user.recs) > 0){
      if(length(user.recs) > 50){
        user.recs <- user.recs[1:50]
      }
      num <- sum(sub$user_id %in% user.recs)            
      num.10 <- sum(sub$user_id %in% user.recs[1:10])
      recall <- num / nrow(sub)
      precision <- num / length(user.recs)
      precision.10 <- num.10 / length(user.recs) 
      recall.10 <- num.10 / nrow(sub)
      f <- 2/(1/recall + 1 / precision)
      f.10 <- 2/(1/recall.10 + 1/precision.10)
      out <- data.frame(recall, recall.10, 
        precision, precision.10, f, f.10)
    } else {
      out <- data.frame(recall = 0, recall.10 = 0, 
        precision = 0, precision.10 = 0, f = 0, f.10 = 0)
    }
  })      
out <- apply(score[,-1], 2, mean) 
out.path <- paste0(recs.path, "scoresCF.csv")      
write.csv(out, file = out.path, row.names = FALSE, quote = FALSE)
