library(wordcloud)
library(stringr)

dat <- read.csv(file = "data/OneMonthOfThree/topic-word-weights.250.csv",
 header = FALSE)
names(dat) <- c("topic.id", "p.word.g.topic", "word.count", "word")
dat$word <- str_trim(as.character(dat$word))


con <- file("data/OneMonthOfThree/topic-keys.250.txt") 
open(con)
results.list <- list();
current.line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    results.list[[current.line]] <- strsplit(line, split = "\\t")
    current.line <- current.line + 1
} 
close(con)

top20w <- ldply(results.list, function(x){
    data.frame(topic = as.numeric(x[[1]][1]) + 1,
      weigth = x[[1]][2],
      word = do.call(c, strsplit(x[[1]][3], split = " ")))
  })


con <- file("data/One_month/topics_questions.txt") 
open(con)
results.list <- list();
current.line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
   if(length(line)){
    results.list[[current.line]] <- line
    current.line <- current.line + 1
  }
} 
close(con)

selected.qs <- ldply(results.list, function(x){
  aux <- strsplit(gsub("\\s+", " ", x), split = " ")[[1]]
  cbind(aux[1], aux[-1][1:(length(aux)-1) %% 2 == 1],
  aux[-1][1:(length(aux)-1) %% 2 == 0])
  })
names(selected.qs) <- c("question_id", "topic", "weigth")
selected.qs$weigth <- as.numeric(as.character(selected.qs$weigth))

top20w$weigth <- as.numeric(as.character(top20w$weigth))
top20w <- arrange(top20w, weigth, decreasing = T)
topics.sub <- unique(top20w[,c("topic", "weigth")])[1:10, "topic"]

top20w.sub <- top20w[top20w$topic %in% topics.sub, ]
cols = data.frame(cols = c("salmon", "deeppink", "darkorange", 
  "darkgreen", "darkblue",
  "darkred", "darkcyan", "black", "red", "lightskyblue"), topic = topics.sub)
top20w.sub.1 <- join(top20w.sub, cols)
wordcloud(words = top20w.sub.1$word, freq = top20w.sub.1$weigth,
  colors = as.character(top20w.sub.1$cols),
  scale=c(1.5,1), random.color=FALSE,
  random.order = TRUE, ordered.colors = TRUE,
  rot.per=.3,use.r.layout=FALSE)


int.top <- dat[dat$topic.id %in% c(29,36,26,2,33), ]
cols <- data.frame(cols = c("red", "orange", 
   "darkgreen", "violet", "navy"), topic.id = c(29,36,26,2,33))
int.top.1 <- join(int.top, cols)
int.top.1 <- ddply(int.top.1, "topic.id", transform, 
  max.top = max(p.word.g.topic))
png("graphs/word_cloud.png", width = 500, height = 500,
  bg = "transparent")
wordcloud(words = int.top.1$word, 
  freq = int.top.1$p.word.g.topic / int.top.1$max.top,
  colors = as.character(int.top.1$cols),
  scale=c(3,.5), random.color=FALSE,
  random.order = FALSE, ordered.colors = TRUE,
  rot.per=.3,use.r.layout=FALSE)
dev.off()



int.top <- dat[dat$topic.id %in% c(36), ]
cols <- data.frame(cols = c("darkorange"), topic.id = c(36))
int.top.1 <- join(int.top, cols)
int.top.1 <- ddply(int.top.1, "topic.id", transform, 
  max.top = max(p.word.g.topic))
png("graphs/word_cloud_36.png", width = 500, height = 500,
  bg = "transparent")
wordcloud(words = int.top.1$word, 
  freq = int.top.1$p.word.g.topic / int.top.1$max.top,
  colors = as.character(int.top.1$cols),
  scale=c(3,.5), random.color=FALSE,
  random.order = FALSE, ordered.colors = TRUE,
  rot.per=.3,use.r.layout=FALSE)
dev.off()

png("graphs/word_cloud_29.png", width = 500, height = 500,
  bg = "transparent")
int.top <- dat[dat$topic.id %in% c(29), ]
cols <- data.frame(cols = c("black"), topic.id = c(29))
int.top.1 <- join(int.top, cols)
int.top.1 <- ddply(int.top.1, "topic.id", transform, 
  max.top = max(p.word.g.topic))

wordcloud(words = int.top.1$word, 
  freq = int.top.1$p.word.g.topic / int.top.1$max.top,
  colors = as.character(int.top.1$cols),
  scale=c(3,.5), random.color=FALSE,
  random.order = FALSE, ordered.colors = TRUE,
  rot.per=.3,use.r.layout=FALSE)
dev.off()


