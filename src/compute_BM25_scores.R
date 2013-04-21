#!/usr/bin/env Rscript

# To run a script from the command line
### add that (up) line
### chmod +x to make it excecutable
### give an explicit path to the file
### run from the command line
### look for getopt r library or commandArgs()

# Example of how to run from command line
# ./compute_BM25_scores.R -o '../data/test.csv'
# -tw '../data/OneMonthOfThree/SparseTopicWeights/top-n-doc-topic-weights.train.250.csv' 
# -sm '../data/OneMonthOfThree/matrix.csv'


# bigalgebra is not on CRAN so run the following to install
# install.packages("bigalgebra", repos="http://R-Forge.R-project.org")

library(Matrix)
library(plyr)
library(bigmemory)
library(biganalytics)
library(bigalgebra)
library(getopt)

# Change this accordingly
# The path should be to the sparse topics weights

topic.wiegths.path <- "../data/OneMonthOfThree/SparseTopicWeights/top-n-doc-topic-weights.train.550.csv"
score.matrix.path <- "../data/OneMonthOfThree/matrix.csv"
# output.path <- "../data/Recs/50_1.6_1.6_0.25_250.csv"

# ======================================
# = Read options from the command line =
# ======================================

# spec <- matrix(c('TWP', 'tw', "a", "character",
#   'SMP', 'sm', "a", "character",
#   'OP', 'o', "a", "character"
#   ), byrow=TRUE, ncol = 4)
# opt <- getopt(spec)

# topic.wiegths.path <- opt$TWP
# score.matrix.path <- opt$SMP
# output.path <- opt$OP


### compute_user_features.R script is assumed to be on the same folder as 
### this script
source("compute_user_features.R", echo = TRUE)

generalized.which.max <- function(x, n){
  out <- rep(NA, n)
  m <- min(x)
  for(k in 1:n){
    out[k] <- which.max(x)
    x[out[k]] <- m
  }
  out
}

meltish <- function(mat){
  cbind(col = rep(colnames(mat),   nrow(mat)),
  row = rep(rownames(mat), each = ncol(mat)),
   c(t(mat)))
}

# ==================
# = Read test data =
# ==================

q.topic.test <- read.csv(file = 
  "../data/OneMonthOfThree/doc-topic-weights.test.550.csv")
ids.test <- q.topic.test[,"question_id"]

# ===============
# = Read alphas =
# ===============

con <- file("../data/OneMonthOfThree/topic-keys.550.txt") 
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

# ===============
# = BM25 scores =
# ===============

user.topic.mat <- as.matrix(user.topic.mat)
neg.ind <- user.topic.mat < 0 
user.topic.mat[neg.ind] <- 0

q.topic.test.mat <- as.matrix(q.topic.test[, -1])
rm(q.topic.test)
alpha.aux <- as.numeric(alphas[,2])

avdl <- mean(apply(user.topic.mat, 1, sum))
### BM25 scores

RecsBM25 <- function(k1, k3, b, n.recs, ids){    
   term.1 <- (k1 + 1) * user.topic.mat /
    (k1 * ((1 - b) + b * apply(user.topic.mat, 1, sum) / avdl) + user.topic.mat)

  term.2 <- apply( (k3 + 1) *  q.topic.test.mat / (k3 + q.topic.test.mat),
                   1, function(x) x * -log(alpha.aux) )
  term.1 <- as.big.matrix(term.1)
  term.2 <- as.big.matrix(term.2)
  sim <- term.1 %*% term.2
  big.recs <- as.big.matrix(apply(sim, 2, function(x.col){
      ind <- generalized.which.max(x.col, n.recs)
      c(ind, x.col[ind])
    }))
  big.recs.ind <- big.recs[1:n.recs,]
  big.recs.scores <- t(big.recs[(n.recs + 1):nrow(big.recs),])
  big.recs.ind <- apply(big.recs.ind, 1, function(x){
    aux.user.id[x, "user_id"]
    })
  rownames(big.recs.ind) <- ids
  rownames(big.recs.scores) <- ids
  cbind(meltish(big.recs.ind), meltish(big.recs.scores)[,2])
}

grid.aux <- expand.grid(b = 0.4, k3 = 0.1, k1 = 3)
for(no.it in 1:nrow(grid.aux)){
  print(no.it)
  print(Sys.time())
  recs <- RecsBM25(grid.aux[no.it,"k1"], grid.aux[no.it,"k3"],
    grid.aux[no.it,"b"], 50, ids.test)
  colnames(recs) <- c("question_id", "user_id", "score")
  output.path <- paste("../data/OneMonthOfThree/Recs/",paste(550, 
    grid.aux[no.it,"k1"], 
    grid.aux[no.it,"k3"], grid.aux[no.it,"b"], 50, sep = "_"), ".csv", sep = "")
  write.csv(recs, file = output.path, row.names = FALSE, quote = FALSE)
}
