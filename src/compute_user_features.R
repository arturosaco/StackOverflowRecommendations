
# =============
# = Read data =
# =============

q.score <- read.csv(file = score.matrix.path)
q.topic.m <- read.csv(file = topic.wiegths.path)
train.ids <- read.table(file = 
  "../data/OneMonthOfThree/itemID_train.txt")
test.ids <- read.table(file = 
  "../data/OneMonthOfThree/itemID_test.txt")

# Restrict the score matrix to the training portion

q.score <- q.score[q.score$question_id %in% train.ids[,1], ]

no.qs.num <- tapply(q.score$question_id, q.score$user_id, length)
q.score.sub <- q.score[q.score$user_id %in% 
   names(no.qs.num)[no.qs.num >= 4], ]

# write.csv(unique(q.score.sub$user_id), file = "../data/denominator.csv",
#   row.names = FALSE, quote = FALSE)


# ============================
# = Generate sparse matrices =
# ============================

### Question-topic matrix

q.topic.m <- q.topic.m[order(as.numeric(q.topic.m[,"question_id"])),]
q.topic.m.sub <- q.topic.m[q.topic.m$question_id %in% q.score.sub$question_id, ]
q.topic.mat <- sparseMatrix(i = as.numeric(q.topic.m.sub[,"question_id"]),
  j = as.numeric(q.topic.m.sub[,"topic"]) + 1,
  x = as.numeric(q.topic.m.sub[,"weight"]))

### User-score matrix

### The object aux.user.id should be saved for translating to/from
### user_id's to rows in the user feature matrix (and so on...)


aux.user.id <- data.frame(user_id = unique(q.score.sub$user_id), 
  user_id.aux = 1:length(unique(q.score.sub$user_id)))
q.score.1 <- join(q.score.sub, aux.user.id)
q.score.1 <- q.score.1[order(q.score.1$question_id), ]

q.score.mat <- sparseMatrix(i = q.score.1$question_id,
  j = q.score.1$user_id.aux, x = q.score.1$score)

user.topic.mat <- t(q.score.mat) %*% q.topic.mat
rm(q.score, q.score.mat, test.ids, train.ids, q.score.1, q.topic.m,
  q.topic.m.sub, q.score.sub)
gc()


