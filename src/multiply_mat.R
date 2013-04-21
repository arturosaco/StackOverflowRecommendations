library(ProjectTemplate)
load.project()
library(Matrix)


meltish <- function(mat){
  cbind(col = rep(colnames(mat),   nrow(mat)),
  row = rep(rownames(mat), each = ncol(mat)),
   c(t(mat)))
}

# ======================
# = Read training data =
# ======================

q.score <- read.csv(file = "data/Three_months/matrix.csv")
q.topic <- read.csv(file = "data/Three_months/doc-topic-weights.train.100.csv")

train.ids <- read.table(file = 
  "data/Three_months/itemID_train.txt")

test.ids <- read.table(file = 
  "data/Three_months/itemID_test.txt")

q.score.train <- q.score[q.score$question_id %in% train.ids[,1], ]

# =============
# = IDs tests =
# =============

length(unique(q.score.train$question_id))
length(unique(q.topic$question_id))

length(intersect(q.score.train$question_id, q.topic$question_id))

interscetion.ids <- intersect(q.score.train$question_id, q.topic$question_id)

# =========================
# = Question-Topic matrix =
# =========================

rownames(q.topic) <- q.topic$question_id
q.topic.m <- meltish(q.topic[q.topic$question_id %in% interscetion.ids, -1])
colnames(q.topic.m) <- c("topic", "question_id", "weight")
q.topic.m <- as.data.frame(q.topic.m)
q.topic.m$topic <- as.numeric(gsub("[^0-9]", "", as.character(q.topic.m$topic)))
q.topic.m$question_id <- as.numeric(as.character(q.topic.m$question_id))
q.topic.m$weight <- as.numeric(as.character(q.topic.m$weight))

q.topic.m <- q.topic.m[order(q.topic.m$question_id),]
q.topic.mat <- sparseMatrix(i = q.topic.m$question_id,
  j = q.topic.m$topic + 1,
  x = q.topic.m$weight)

# =========================
# = Question-User matrix =
# =========================

aux.user.id <- data.frame(user_id = unique(q.score.train$user_id), 
  user_id.aux = 1:length(unique(q.score.train$user_id)))
q.score.train.1 <- join(q.score.train, aux.user.id)
q.score.train.1 <- q.score.train.1[order(q.score.train$question_id), ]
q.score.mat <- sparseMatrix(i = q.score.train.1$question_id,
  j = q.score.train.1$user_id.aux, x = q.score.train.1$score)

user.topic.mat <- t(q.score.mat) %*% q.topic.mat

# ==================
# = Read test data =
# ==================

q.topic.test <- read.csv(file = 
  "data/Three_months/doc-topic-weights.test.100.csv")











############################################

# Sanity check that users with all zero rows in the mat product
# correspond to users with all zero scores

ids.0 <- (1:length(unique(q.score.sub$user_id)))[apply(user.topic.mat, 1, 
  function(x) sum(x == 0)) == 50]
aux.user.id[aux.user.id$user_id.aux %in% ids.0,]$user_id

qids.0 <- q.score.sub[q.score.sub$user_id %in%
 aux.user.id[aux.user.id$user_id.aux %in% ids.0,]$user_id, "question_id"]

############################################


# Choose a subset of users with the best scores 
score.summary <- ddply(q.score.sub, "user_id", summarise, tot.score = sum(score),
  .progress = "text")
ids.sub <- score.summary[score.summary$tot.score > 4, "user_id"]


user.topic.mat.sub <- user.topic.mat[aux.user.id[aux.user.id$user_id %in%
   ids.sub, "user_id.aux"], ]


# ==================
# = Read test data =
# ==================

meltish <- function(mat){
  cbind(col = rep(colnames(mat),   nrow(mat)),
  row = rep(rownames(mat), each = ncol(mat)),
   c(t(mat)))
}

generalized.which.max <- function(x, n){
  out <- rep(NA, n)
  m <- min(x)
  for(k in 1:n){
    out[k] <- which.max(x)
    x[out[k]] <- m
  }
  out
}

get.recs <- function(user.topic.matrix.internal, question.subset.index, no.recs){
  sub.new.q <- as.matrix(q.topic.test[question.subset.index, -1])
  rownames(sub.new.q) <- q.topic.test[question.subset.index, 1]
  sim.mat <- user.topic.matrix.internal %*% t(sub.new.q)
  recs <- apply(sim.mat, 2, generalized.which.max, no.recs)
  recs <- apply(recs, 1, function(x){
    aux.user.id$user_id[x]
  })
  rownames(recs) <- q.topic.test[question.subset.index, "question_id"]
  colnames(recs) <- no.recs:1
  recs <- meltish(recs)
  colnames(recs) <- c("score", "question_id", "user_id")
  recs
}


q.topic.test <- read.csv(file = 
  "data/Three_months/doc-topic-weights.test.100.csv")


indices <- list()
for(k in 1:20){
  indices[[k]] <- ((k - 1) * 1000 + 1):(k * 1000)
}
recs.list <- lapply(indices, function(x.indices) {
   print(range(x.indices))
   get.recs(user.topic.mat, x.indices, 50)
 })

recs <- do.call(rbind, recs.list)

write.csv(recs[,c("question_id", "user_id", "score")],
 file = "logs/recommendations.csv", row.names = FALSE)