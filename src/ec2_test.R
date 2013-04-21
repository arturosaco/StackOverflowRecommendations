library(ProjectTemplate)
load.project()
library(Matrix)

estimatePi <- function(numDraws){
  r <- .5 #radius... in case the unit circle is too boring
  x <- runif(numDraws, min=-r, max=r)
  y <- runif(numDraws, min=-r, max=r)
  inCircle <- ifelse( (x^2 + y^2)^.5 < r , 1, 0)
  return(sum(inCircle) / length(inCircle) * 4)
}

 
# ==========
# = Mapper =
# ==========


trimWhiteSpace <- function(line) gsub("(^ +)|( +$)", "", line)
splitIntoWords <- function(line) unlist(strsplit(line, "[[:space:]]+"))
 
## **** could do with a single readLines or in blocks
con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  line <- trimWhiteSpace(line)
  cat(as.numeric(line),"\t", "\n", sep="")
}
 
close(con)

# ===============
# = parseEMRout =
# ===============

# this starts the job assuming you have your credentials.json set up properly
# it also assumes the mapper.R/reducer.R/calculatePiFunction.R are all in a bucket called emrexample
# Output goes to emrout on S3 which must NOT exist before this is run
# the file numberList.txt is not in this gist because it is 10,000 lines long: each line is simply an integer from 1:10000
# numberList.txt needs to be created and placed in your S3 emrexample bucket
# you will also need the Amazon EMR command line tools: http://docs.amazonwebservices.com/ElasticMapReduce/latest/DeveloperGuide/DownloadingtheCLI.html
# and S3CMD: http://s3tools.org/s3cmd
 
system("elastic-mapreduce --create --stream --input s3n://emrexample/numberList.txt --mapper s3n://emrexample/mapper.R --reducer s3n://emrexample/reducer.R --output s3n://emrout/ --name EMRexample --num-instances 50 --cache s3n://emrexample/calculatePiFunction.R#calculatePiFunction.R")
 
########### Don't run the rest of this until the job is done ########################
#you have to have s3cmd for this to work
#copies the results back
system("s3cmd get s3://emrexample/out/* .")
 
require(Hmisc) #for the substring.location() function
 
#be sure and change this path...
basePath <- "/home/jal/Documents/R/EMR Example/output/"
 
fileList <- list.files(path=basePath)
 
fi <- 1
fileResults <- NULL

for (fi in 1:length(fileList)){
fname <- paste(basePath, fileList[fi], sep = "")
tst <- readChar(fname, file.info(fname)$size)
spt <- strsplit(tst, "|", fixed=T)
 
singleFileResults <- NULL
 
for (i in 1:(length(spt[[1]])-1)) {
spt2 <- substr(spt[[1]][i], substring.location(spt[[1]][i], "\tA")$first+1, nchar(spt[[1]][i]))
results <- unserialize(charToRaw(spt2))
singleFileResults[[i]] <- results
}
 
fileResults[[fi]] <- singleFileResults
}
 
f <- unlist(fileResults)
 
cat("estimate of pi is: ", mean(f), "\n")

# ===========
# = Reducer =
# ===========

#! /usr/bin/env Rscript
 
options(warn=-1)
trimWhiteSpace <- function(line) gsub("(^ +)|( +$)", "", line)
 
con <- file("stdin", open = "r")
source("./calculatePiFunction.R")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
x <- as.numeric(trimWhiteSpace(line))
set.seed(x) 
myOutput <- estimatePi(1e5) 
cat(line, rawToChar(serialize(myOutput, NULL, ascii=T)), "|\n", sep = "")
}
 
close(con)

# ===============
# = Using segue =
# ===============

library(segue)
setCredentials(awsAccessKeyText = "AKIAJRLJABRMPTNWAV3A",
  awsSecretKeyText = "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")

myCluster   <- createCluster(numInstances=5,
  cranPackages=c("Hmisc", "plyr"))

myList <- NULL
set.seed(1)
for (i in 1:10){
   a <- c(rnorm(999), NA)
   myList[[i]] <- a
   }
outputLocal  <- lapply(myList, mean, na.rm=T)
outputEmr   <- emrlapply(myCluster, myList, mean,  na.rm=T)
all.equal(outputEmr, outputLocal)


stopCluster(myCluster)