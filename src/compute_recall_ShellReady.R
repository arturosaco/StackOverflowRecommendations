#!/usr/bin/env Rscript

recs.path <- "data/OneMonthOfThree/Recs/"

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

train.num.qs <- tapply(score.train$question_id, score.train$user_id, length)

score.q.test <- score.test[score.test$user_id %in% 
  names(train.num.qs)[train.num.qs >= 4],]

#files <- dir(recs.path)
files <- union(dir(paste(recs.path, "/summaries", sep = "")), dir(recs.path))
files <- paste(recs.path, grep("^[0-9].*_.*_.*_.*_.*.csv", 
  files, value = TRUE), sep = "")

library(multicore)
library(plyr)
library(reshape2)
library(ggplot2)

scores <- lapply(
  files,
  function(file.x) {
    if (!(gsub(recs.path, "", file.x) %in% 
        dir(paste(recs.path, "summaries/", sep = "/")))) {
      recs <- as.matrix(read.csv(file = file.x))
      recs <- recs[order(recs[,"question_id"]), ]
      recs.1 <- cbind(question_id = sort(unique(recs[,"question_id"])), 
        matrix(recs[,2], ncol = 50, nrow = nrow(recs) / 50, byrow = TRUE))
      score <- ddply( score.q.test, "question_id",
        function(sub) {
          num <- sum(sub$user_id %in% 
            recs.1[recs.1[,"question_id"] == unique(sub$question_id), -1]) 
          num.10 <- sum(sub$user_id %in% 
            recs.1[recs.1[,"question_id"] == unique(sub$question_id), 2:11])
          recall <- num / nrow(sub)
          precision <- num / 50
          precision.10 <- num.10 / 10 
          recall.10 <- num.10 / nrow(sub)
          f <- 1/(1/recall + 1/precision)
          f.10 <- 1/(1/recall.10 + 1/precision.10)
          data.frame(recall, recall.10, 
            precision, precision.10, f, f.10)
        })
      
      out <- c(unlist(strsplit(gsub(paste(
          recs.path, "\\.csv.*", sep = "|"),
                                        "",
                                        file.x ),
                                  split = "_" )),
                apply(score[,-1], 2, mean) )
      
      out.path <- paste0(recs.path, "summaries/", 
                          gsub(paste(recs.path, "\\.csv.*", sep = "|"), "", file.x),
                          ".csv" )
      
      write.csv(out, file = out.path, row.names = FALSE, quote = FALSE)
    } else {
      out <- read.csv( paste0( recs.path, "summaries/", 
                               gsub(recs.path, "", file.x) ))[,1]
    }
    out
  })

scores.df <- data.frame(do.call(rbind, scores))

names(scores.df) <- c( "no.topics", "k1", "k3", "b", "no.recs", "recall",
                       "recall.10", "precision", "precision.10", "f", "f.10" )

scores.df$f <- 2* as.numeric(as.character(scores.df$f))
scores.df$f.10 <- 2* as.numeric(as.character(scores.df$f.10))


# =============================
# = Check list of data points =
# =============================

expand.grid(k1 = 3:7, k3 = c(0.025, 0.05,  0.075, 0.1, 0.125, 0.15),
  b = seq(0,1,0.1))

check <- scores.df[scores.df$no.topics == 250, c("k1", "k3", "b")]
nrow(check[check$k1 %in% 3:7 & 
  check$k3 %in% c(0.025, 0.05,  0.075, 0.1, 0.125, 0.15) &
  check$b %in% seq(0,1,0.1), ])

###############################

scores.df.m <- melt( scores.df, 
                     id.vars = c("no.topics", "k1", "k3", "b",  "no.recs") )
scores.df.m$variable <- as.character(scores.df.m$variable)
scores.df.m[scores.df.m$variable == "recall", "variable"] <- "recall.50"
scores.df.m[scores.df.m$variable == "precision", "variable"] <- "precision.50"
scores.df.m[scores.df.m$variable == "f", "variable"] <- "f.50"
scores.df.m$variable <- factor(scores.df.m$variable,
  levels = c("recall.10", "recall.50", "precision.10", "precision.50",
    "f.10", "f.50"))

scores.df.m$value <- as.numeric(as.character(scores.df.m$value))
scores.df.m$k3 <- as.numeric(as.character(scores.df.m$k3))
# ============
# = Big plot =
# ============
png(file = "graphs/big_one.png", height = 450, width = 900)
ggplot(scores.df.m[scores.df.m$no.topics == 250 & 
  !(scores.df.m$b %in% c(0.25, 0.75)) & 
  scores.df.m$k1 %in% 3:7,], aes(x =k3 , 
                        y = value * 100, group = k1, colour = k1)) + geom_line() + 
  facet_grid(variable ~ b, scales = "free") + geom_point(size = 1) +
  theme_bw() + theme(axis.text.x=element_text(angle=-90)) +
  ylab("")
dev.off()
### Selected params; b = 0.4, k1=3, k3 = 0.1
# =====================
# = Interactions plot =
# =====================

## fixed k1, k3

ggplot(scores.df.m[scores.df.m$no.topics == 250 & 
  scores.df.m$k1 ==3 & 
  scores.df.m$k3 == 0.1,], aes(x = b, y =100 *  value)) + 
  geom_point() + facet_grid(variable~., scales = "free") + geom_line() + 
  ggtitle("k1 = 3, k3 = 0.1") + theme_bw() + ylab("") +
  geom_vline(x = 0.4, colour = "salmon") + scale_x_continuous(breaks = 0.4)

## fixed k1, b

ggplot(scores.df.m[scores.df.m$no.topics == 250 & 
  scores.df.m$k1 ==3 & 
  scores.df.m$b == 0.4,], aes(x = k3, y =100 *  value)) + 
  geom_point() + facet_grid(variable~., scales = "free") + geom_line() + 
  ggtitle("k1 = 3, b = 0.4") + theme_bw() + ylab("") + 
  geom_vline(x = 0.1, colour = "salmon") + scale_x_continuous(breaks = 0.1)

## fixed k3, b

ggplot(scores.df.m[scores.df.m$no.topics == 250 & 
  scores.df.m$k3 ==0.1 & 
  scores.df.m$b == 0.4,], aes(x = k1, y =100 *  value)) + 
  geom_point() + facet_grid(variable~., scales = "free") + geom_line() + 
  ggtitle("k3 = 0.1, b = 0.4") + theme_bw() + ylab("") + 
  geom_vline(x = 3, colour = "salmon") + scale_x_continuous(breaks = 3)


aux.1 <- scores.df.m[scores.df.m$no.topics == 250 & 
  scores.df.m$k1 ==3 & 
  scores.df.m$k3 == 0.1,]
aux.2 <- scores.df.m[scores.df.m$no.topics == 250 & 
  scores.df.m$k1 ==3 & 
  scores.df.m$b == 0.4,]
aux.3 <- scores.df.m[scores.df.m$no.topics == 250 & 
  scores.df.m$k3 ==0.1 & 
  scores.df.m$b == 0.4,]

### in all aux call the plotting var "var.aux" 
aux.1$variable <- as.character(aux.1$variable)
aux.2$variable <- as.character(aux.2$variable)
aux.3$variable <- as.character(aux.3$variable)

aux.1$id <- 1
aux.2$id <- 2
aux.3$id <- 3

names(aux.1) <- c("no.topics", "k1", "k3", "b", "no.recs",
 "measure","measure.val", "id")
aux.1.m <- melt(aux.1, id.vars = c("measure", "measure.val", "id"))

names(aux.2) <- c("no.topics", "k1", "k3", "b", "no.recs",
 "measure","measure.val", "id")
aux.2.m <- melt(aux.2, id.vars = c("measure", "measure.val", "id"))

names(aux.3) <- c("no.topics", "k1", "k3", "b", "no.recs",
 "measure","measure.val", "id")
aux.3.m <- melt(aux.3, id.vars = c("measure", "measure.val", "id"))

aux.4 <- rbind(aux.1.m[aux.1.m$variable == "b", ],
  aux.2.m[aux.2.m$variable == "k3", ],
  aux.3.m[aux.3.m$variable == "k1", ])
aux.4$max.val <- NA
aux.4[aux.4$variable == "b", "max.val"] <- 0.4
aux.4[aux.4$variable == "k3", "max.val"] <- 0.1
aux.4[aux.4$variable == "k1", "max.val"] <- 3

aux.4$measure <- factor(aux.4$measure, levels =
  c("recall.10", "recall.50", "precision.10", "precision.50",
    "f.10", "f.50"))
png(file = "graphs/interaction_plot.png", height = 450, width = 550)
ggplot(aux.4, aes(x = value, y =100 *  measure.val)) + 
  geom_point() + facet_grid(measure~variable, scales = "free") + geom_line() + 
   theme_bw() + xlab("Parameter value") + ylab("") +
   geom_vline(data = aux.4, aes(xintercept = max.val), colour = "salmon") 
dev.off()

# =============
# = zoom plot =
# =============
ggplot(scores.df.m[scores.df.m$no.topics == 250 & 
    scores.df.m$b %in% c(0.4, 0.3, 0.5) & scores.df.m$variable == "f.10" &
    scores.df.m$k1 %in% 3:7, ],
     aes(x =k3, y = value * 100, group = k1, colour = k1)) + geom_line() + 
  geom_point() + facet_grid(variable ~ b, scales = "free_y") +
  theme_bw() + theme(axis.text.x=element_text(angle=-90)) + 
  ylab("F1@10")

ggplot(scores.df.m[scores.df.m$no.topics == 250 & 
    scores.df.m$b %in% c(0.4, 0.3, 0.5) & scores.df.m$variable == "recall.10" &
    scores.df.m$k1 %in% 3:7, ],
     aes(x =k3, y = value * 100, group = k1, colour = k1)) + geom_line() + 
  geom_point() + facet_grid(variable ~ b, scales = "free_y") +
  theme_bw() + theme(axis.text.x=element_text(angle=-90)) + 
  ylab("recall@10")

# =============
# = no.topics =
# =============
scores.df.m$var.aux.1 <- gsub(".*\\.", "", scores.df.m$variable)
scores.df.m$var.aux.2 <- gsub("\\..*", "", scores.df.m$variable)
scores.df.m$var.aux.2 <- factor(scores.df.m$var.aux.2, levels = 
  c("recall", "precision", "f"))

png(file = "graphs/no_topics.png", height = 300, width = 350)
ggplot(scores.df.m[scores.df.m$k1 == 5 & scores.df.m$k3 == 0.1 &
  scores.df.m$b == 0.25, ], aes(x = as.numeric(as.character(no.topics)),
   y = value * 100  , group = 1)) + theme_bw() +
  geom_line() + facet_grid(var.aux.2 ~ var.aux.1, scales = "free_y") +
  ylab("") + xlab("Number of topics")
dev.off()


scores.df.m[scores.df.m$no.topics == 250 & 
    scores.df.m$b %in% c(0.4) & 
    scores.df.m$k1 %in% 3 & scores.df.m$k3 == 0.1, c("variable", "value")] 