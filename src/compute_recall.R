recs.path <- "data/Recs_5plus/fill0/"
test.path <- "data/OneMonthOfThree/matrix_test.csv"

files <- union(dir(paste(recs.path, "/summaries", sep = "")), dir(recs.path))
files <- paste(recs.path, grep(".csv", files, value = TRUE), sep = "")
score.q.test <- read.csv(test.path)

library(multicore)
library(plyr)
library(ggplot2)

scores <- mclapply(files,
  function(file.x){
  ### check if file.x is in summaries here, compute statistics
  if(!(gsub(recs.path, "", file.x) %in%
   dir(paste(recs.path, "summaries", sep = "/")))){
    recs <- as.matrix(read.csv(file = file.x))
    recs <- recs[order(recs[,"question_id"]), ]
    recs.1 <- cbind(question_id = sort(unique(recs[,"question_id"])), 
      matrix(recs[,2], ncol = 50, nrow = nrow(recs) / 50, byrow = TRUE))
    score <- ddply(score.q.test, "question_id", function(sub){
          num <- sum(sub$user_id %in% recs.1[recs.1[,"question_id"] == 
            unique(sub$question_id), -1]) 
          num.10 <- sum(sub$user_id %in% recs.1[recs.1[,"question_id"] == 
            unique(sub$question_id), 2:11])
          recall <- num / nrow(sub)
          precision <- num / 50
          precision.10 <- num.10 / 10 
          recall.10 <- num.10 / nrow(sub)
          f <- 1/(1/recall + 1/precision)
          f.10 <- 1/(1/recall.10 + 1/precision.10)
          data.frame(recall, recall.10, precision, precision.10, f, f.10)
        })
    out <- c(unlist(strsplit(
      gsub(paste(recs.path, "\\.csv.*", sep = "|"), "", file.x), split = "_") ),
      apply(score[,-1], 2, mean))
    out.path <- paste(recs.path, "summaries/", 
      gsub(paste(recs.path, "\\.csv.*", sep = "|"), "", file.x),
      ".csv", sep = "")
    write.csv(out, file = out.path)
    out
  }else{
    out <- read.csv(paste(recs.path, "summaries/", 
      gsub(recs.path, "", file.x), sep = ""))
    out <- out[,2]
  }
  out
 }, mc.cores = 4)

scores.df <- data.frame(do.call(rbind, scores))
names(scores.df) <- c("no.topics", "k1", "k3", "b", "no.recs", "recall",
  "recall.10", "precision", "precision.10", "f", "f.10")
print(scores.df, row.names = FALSE)

scores.df.m <- melt(scores.df, 
  id.vars = c("no.topics", "k1", "k3", "b",  "no.recs"))

scores.df.m$value <- as.numeric(as.character(scores.df.m$value))
scores.df.m$k3 <- as.numeric(as.character(scores.df.m$k3))
ggplot(scores.df.m, aes(x =k3 , 
  y = value * 100, group = k1, colour = k1)) + geom_line() + 
  geom_point() + facet_grid(variable ~ b, scales = "free_y") +
  theme_bw()
