library(ProjectTemplate)
load.project()

scores <- read.csv("data/questions.csv")
scores.odd <- read.csv("data/odd.csv")
scores.even <- read.csv("data/even.csv")

scores <- unique(scores)

scores$answer_body <- as.character(scores$answer_body)
scores$question_body <- as.character(scores$question_body)
scores$score <- as.numeric(as.character(scores$score))

# =================
# = Answer length =
# =================

dcast(ddply(scores, "is_accepted", function(sub){
    aux <- summary(nchar(sub$answer_body))
    data.frame(q = names(aux), v = as.numeric(as.character(aux)))
  }), q ~ is_accepted, value.var = "v")

# ==================================
# = Number of answers per question =
# ==================================

no.answers <- ddply(scores, "question_id", summarise,
  no.answers = length(answer_id))
round(prop.table(table(no.answers$no.answers)) * 100)

# =================
# = Answer scores =
# =================

plot(table(scores$score))


processed.text.answ <- ddply(scores, "answer_id", function(sub){
    text <- unique(sub$answer_body)
    html.thing <- htmlParse(text, asText = TRUE)
    plain.text <- paste(xpathSApply(html.thing, "//p", xmlValue), collapse = " ")
    code <- paste(xpathSApply(html.thing, "//pre", xmlValue), collapse = " ")
    data.frame(answer_id = sub$answer_id, answ.plain.text = plain.text,
      answ.code = code)
  }, .progress = "text")

processed.text.q <- ddply(scores, "question_id", function(sub){
    text <- unique(sub$question_body)
    html.thing <- htmlParse(text, asText = TRUE)
    plain.text <- paste(xpathSApply(html.thing, "//p", xmlValue), collapse = " ")
    code <- paste(xpathSApply(html.thing, "//pre", xmlValue), collapse = " ")
    data.frame(question_id = unique(sub$question_id), q.plain.text = plain.text,
      q.code = code)
  }, .progress = "text")

scores.1 <- join(scores, processed.text.answ)
scores.2 <- join(scores.1, processed.text.q)

answ.text <- scores.2[, c("answer_id", "is_accepted", "answ.code", "answ.plain.text")]
answ.text.m <- melt(answ.text, id.vars = c("answer_id", "is_accepted"))
answ.text.m$value <- as.character(answ.text.m$value)

answ.sum <- dcast(ddply(answ.text.m, c("variable", "is_accepted"), function(sub){
    aux <- summary(nchar(sub$value))
    data.frame(q = names(aux), v = as.numeric(as.character(aux)))
  }), variable + is_accepted ~ q, value.var = "v")
names(answ.sum) <- c("variable", "is_accepted", "q1", "q3", "max", "mean", "median",
    "min")
ggplot(answ.sum, aes(y = mean, x = variable, ymin = q1, ymax = q3,
  colour = is_accepted)) +
  geom_point() + geom_linerange(size = 1.5, alpha = .4) + coord_flip()

scores.2$answer.has.code <- nchar(as.character(scores.2$answ.code)) > 0
round(prop.table(table(scores.2$answer.has.code)) * 100)

#rows are accepted/not accepted

round(prop.table(table(scores.2$is_accepted, scores.2$answer.has.code), 1) * 100)

# scores$X <- NULL

prop.table(table(scores$is_accepted)) * 100

# ====================
# = Split train/test =
# ====================

q.ids <- unique(scores.sub.2$index.q)
u.ids <- unique(scores.sub.2$index.o)

train.q.ids <- sample(1:length(q.ids), round(2/3 * length(q.ids)))
test.q.ids <- setdiff(q.ids, train.q.ids)

mat <- sparseMatrix(i = scores.sub.2$index.q,
  j = scores.sub.2$index.o,
  x = scores.sub.2$score.1)

# ======================
# = Use recommenderlab =
# ======================

mat.train.r <- new("realRatingMatrix", data = t(mat[train.q.ids, ]))
mat.test.r <- new("realRatingMatrix", data = mat[test.q.ids, ])

rec <- Recommender(mat.train.r,  method = "UBCF")
recom <- predict(object = rec, newdata = mat.train.r, n = 5, type="topNList")

# ==============
# = Wordclouds =
# ==============

library(wordcloud)
library(stringr)

dat <- read.csv(file = "data/topic-word-weights.50.csv", header = FALSE)
names(dat) <- c("topic.id", "p.word.g.topic", "word.count", "word")
dat$word <- str_trim(as.character(dat$word))


con <- file("data/topic-keys.50.txt") 
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


con <- file("data/topics_questions.txt") 
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



# ======================
# = Interesting topics =
# ======================

29
36

26
2
33
# ==========================
# = Your Comment Goes here =
# ==========================

top20w$alpha <- top20w$weigth
aux <- unique(top20w[,c("topic", "alpha")])
aux$topic.id <- aux$topic -1
dat.1 <- join(dat, aux)

aux.2 <- ddply(dat.1, "topic.id", summarise, med = sum(p.word.g.topic), 
  alpha = unique(alpha))
ggplot(aux.2, aes(x = alpha, y = med)) + geom_point()