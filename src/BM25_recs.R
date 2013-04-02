library(ProjectTemplate)
load.project()
library(Matrix)

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

# ======================
# = Read training data =
# ======================

q.score <- read.csv(file = "data/OneMonthOfThree/matrix.csv")
q.topic <- read.csv(file = "data/OneMonthOfThree/doc-topic-weights.train.250.csv")

train.ids <- read.table(file = 
  "data/OneMonthOfThree/itemID_train.txt")

test.ids <- read.table(file = 
  "data/OneMonthOfThree/itemID_test.txt")

q.score.train <- q.score[q.score$question_id %in% train.ids[,1], ]

# =============
# = IDs tests =
# =============

interscetion.ids <- intersect(q.score.train$question_id, q.topic$question_id)

# =========================
# = Question-Topic matrix =
# =========================

rownames(q.topic) <- q.topic$question_id
q.topic.m <- meltish(q.topic[q.topic$question_id %in% interscetion.ids, -1])
colnames(q.topic.m) <- c("topic", "question_id", "weight")
#q.topic.m <- as.data.frame(q.topic.m)
q.topic.m[,"topic"] <- as.numeric(gsub("[^0-9]", "", as.character(q.topic.m[,"topic"])))

q.topic.m <- q.topic.m[order(as.numeric(q.topic.m[,"question_id"])),]
q.topic.mat <- sparseMatrix(i = as.numeric(q.topic.m[,"question_id"]),
  j = as.numeric(q.topic.m[,"topic"]) + 1,
  x = as.numeric(q.topic.m[,"weight"]))

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

# cache("user.topic.mat")
# cache("aux.user.id")

### If the intermediate results are cached, the following two object can
### be loaded directly to avoid computing q.score.mat


load("cache/user.topic.mat.RData")
load("cache/aux.user.id")

# ==================
# = Read test data =
# ==================

q.topic.test <- read.csv(file = 
  "data/OneMonthOfThree/doc-topic-weights.test.250.csv")
ids.test <- q.topic.test[,"question_id"]

# ===============
# = Read alphas =
# ===============


con <- file("data/OneMonthOfThree/topic-keys.250.txt") 
open(con)
results.list <- list();
current.line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    results.list[[current.line]] <- strsplit(line, split = "\\t")[[1]]
    current.line <- current.line + 1
} 
close(con)

alphas <- data.frame(do.call(rbind, results.list)[, c(1, 2)])
names(alphas) <- c("topic", "alpha")
alphas[,"topic"] <- as.numeric(as.character(alphas[,"topic"]))
alphas[,"alpha"] <- as.numeric(as.character(alphas[,"alpha"]))

q.topic.test.norm <- t(apply(q.topic.test[,-1], 1, function(row){
  row * alphas[,2]
  }))



# ========
# = BM25 =
# ========
user.topic.mat <- as.matrix(user.topic.mat)
q.topic.test.mat <- as.matrix(q.topic.test[, -1])
alpha.aux <- as.numeric(alphas[,2])

k1 <- 1
k3 <- 1
b <- 0.5

avdl <- mean(apply(user.topic.mat, 1, sum))

indices <- list()
for(k in 1:28){
  indices[[k]] <- ((k - 1) * 1000 + 1):(k * 1000)
}
indices[[29]] <- 28001:nrow(q.topic.test)

RecsBM25 <- function(k1, k2, b, indices, term.1, term.2, n.recs, ids){    
  term.1 <- ((k1 + 1) * user.topic.mat) / 
    ((k1 * (1 - b) + b * (apply(user.topic.mat, 1, sum) / avdl)) + user.topic.mat)

  term.2 <- apply((((k3 + 1) *  q.topic.test.mat) / 
      (k3 + q.topic.test.mat)), 1, function(x) x * alpha.aux)
  recs.bm.25.0 <- lapply(1:length(indices), function(x){  
    recs.sub <- apply((term.1 %*% term.2[,indices[[x]]]), 2, function(y){
        ind <- generalized.which.max(y, n.recs)
    })
    recs.sub
  })
  recs.bm25.1 <- t(do.call(cbind, recs.bm.25.0))

  recs.bm25.2 <- apply(recs.bm25.1, 2, function(x){
    aux.user.id[x, "user_id"]
    })
  recs.bm25 <- cbind(recs.bm25.2, question_id = ids)
  recs.bm25
}

par.grid <- expand.grid(k1 = c(1, 1.5, 2), k3 = c(1, 1.5, 2),
  b = seq(.25, .75, .25))
for(k in 1:nrow(par.grid)){
  print(k)
  recs <- RecsBM25(par.grid[k,"k1"], par.grid[k,"k3"],
     par.grid[k,"b"], indices, term.1, term.2, 50, ids.test)
  path.temp <- paste("data/Recs/recs_", paste(par.grid[k,], collapse = "_"),
    ".Rdata", sep = "")
  save(recs, file = path.temp)
  gc()
}


score.q.test <- read.csv("data/OneMonthOfThree/matrix_test.csv")
score <- ddply(score.q.test, "question_id", function(sub){
    sum(sub$user_id %in% recs.bm25[recs.bm25[,"question_id"] == 
      unique(sub$question_id), -1])
  }, .progress = "text")

