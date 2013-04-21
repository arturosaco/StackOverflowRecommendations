#!/usr/bin/env Rscript

library(plyr)

root.path <- "output1of3"

score.matrix.path <- paste(root.path, "matrix.csv", sep="/")
train.questions.path <- paste(root.path, "itemID_train.txt", sep="/")
test.questions.path <- paste(root.path, "itemID_test.txt", sep="/")
train.users.path <- paste(root.path, "userID_train.txt", sep="/")
test.users.path <- paste(root.path, "userID_test.txt", sep="/")

score <- read.csv(file = score.matrix.path)
train.questions <- read.table(file = train.questions.path)[,1]
test.questions <- read.table(file = test.questions.path)[,1]
train.users <- read.table(file = train.users.path)[,1]
test.users <- read.table(file = test.users.path)[,1]

score.train <- score[score$question_id %in% train.questions, ]
score.test <- score[score$question_id %in% test.questions, ]
score.CF.test <- score.test[score.test$user_id %in% test.users, ]

train.num.qs <- tapply(score.train$question_id, score.train$user_id, length)

score.train.sub <- score.train[score.train$user_id %in% names(train.num.qs)[train.num.qs >= 4],]
score.test.sub <- score.test[score.test$user_id %in% names(train.num.qs)[train.num.qs >= 4],]
score.CF.test.sub <- score.CF.test[score.CF.test$user_id %in% names(train.num.qs)[train.num.qs >= 4],]

cat( paste0( "Magic number for CF recall/precision calculations: ",
             length(unique(score.CF.test.sub$question_id)),
             "\n" ))

cat( paste0( "Magic number for BM25 recall/precision calculations: ",
             length(unique(score.test.sub$question_id)),
             "\n" ))
