library(ProjectTemplate)
load.project()

scores <- read.csv("data/1_100.csv")
scores$answer_body <- as.character(scores$answer_body)
scores$question_body <- as.character(scores$question_body)

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



dcast(ddply(scores, "is_accepted", function(sub){
    aux <- summary(nchar(sub$answer_body))
    data.frame(q = names(aux), v = as.numeric(as.character(aux)))
  }), q ~ is_accepted, value.var = "v")

txt <- htmlParse(scores[1:3, "question_body"], asText = TRUE)
plain.text <- paste(xpathSApply(txt, "//p", xmlValue), collapse = " ")
code <- paste(xpathSApply(txt, "//pre", xmlValue), collapse = " ")




# scores$X <- NULL

prop.table(table(scores$is_accepted)) * 100

ids <- scores[scores$answer_id %in%
  names(table(scores$answer_id)[table(scores$answer_id) >1]), "question_id"]

scores[scores$question_id %in% ids, ]

texto <- paste(scores$question_body[1:15], collapse = " ")