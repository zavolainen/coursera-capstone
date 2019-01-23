
library(caret)
library(tm) # text mining library
library(quanteda)
set.seed(112) # set seed to be able to reproduce the sampling


setwd("~/Data_Science_Specialization/capstone_project")

dataUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fileName <- "data/Coursera-SwiftKey.zip"
sourceFolder <- "~/Data_Science_Specialization/capstone_project/data/final"

## load the zip if it does not exist
if (!file.exists(fileName)) {
        download.file(dataUrl, fileName, mode = "wb")
        unzip(fileName)
}

list.files("final/en_US")

## read the blogs and twitter files with readLines function
blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8")
twitter <- readLines("./final/en_US/en_US.twitter.txt", warn = FALSE, encoding="UTF-8")
news <- readLines("./final/en_US/en_US.news.txt", warn = FALSE, encoding="UTF-8")

## sample news data
sampSizeN <- floor(0.50 * length(news))
trainN <- sample(seq_len(length(news)), size = sampSizeN)
trainingN <- as.matrix(news[trainN])
testingN <- as.matrix(news[-trainN])

trainingN <- removePunctuation(trainingN) # remove punctuation
trainingN <- tolower(trainingN)

# news corpus and 2-grams and 3-grams
corpusN <- corpus(trainingN)
bigramsN <- dfm(trainingN, ngrams = 2)
bigramsN1000 <- data.frame(topfeatures(bigramsN, 1000))
names(bigramsN1000) <- c("frequency"); bigramsN1000$words <- rownames(bigramsN1000)

trigramsN <- dfm(trainingN, ngrams = 3)
trigramsN1000 <- data.frame(topfeatures(trigramsN, 1000))
names(trigramsN1000) <- c("frequency"); trigramsN1000$words <- rownames(trigramsN1000)


## blogs
#sample blogs data
sampSizeB <- floor(0.50 * length(blogs))
trainB <- sample(seq_len(length(blogs)), size = sampSizeB)
trainingB <- as.matrix(blogs[trainB])
## testingB <- as.matrix(dfBlogs[-trainB,]) ## no need?

trainingB <- removePunctuation(trainingB) # remove punctuation
trainingB <- tolower(trainingB)

corpusB <- corpus(trainingB)
bigramsB <- dfm(trainingB, ngrams = 2)
bigramsB1000 <- data.frame(topfeatures(bigramsB, 1000))
names(bigramsB1000) <- c("frequency"); bigramsB1000$words <- rownames(bigramsB1000)

trigramsB <- dfm(trainingB, ngrams = 3)
trigramsB1000 <- data.frame(topfeatures(trigramsB, 1000))
names(trigramsB1000) <- c("frequency"); trigramsB1000$words <- rownames(trigramsB1000)

## twitter
#sample twitter set (much bigger data so only 20%)
sampSizeT <- floor(0.20 * length(twitter))
trainT <- sample(seq_len(length(twitter)), size = sampSizeT)
trainingT <- as.matrix(twitter[trainT])

trainingT <- removePunctuation(trainingT) # remove punctuation
trainingT <- tolower(trainingT)

corpusT <- corpus(trainingT)
bigramsT <- dfm(trainingT, ngrams = 2)
bigramsT1000 <- data.frame(topfeatures(bigramsT, 1000))
names(bigramsT1000) <- c("frequency"); bigramsT1000$words <- rownames(bigramsT1000)

trigramsT <- dfm(trainingT, ngrams = 3)
trigramsT1000 <- data.frame(topfeatures(trigramsT, 1000))

names(trigramsT1000) <- c("frequency"); trigramsT1000$words <- rownames(trigramsT1000)

## combine news, blogs and twitter bigrams
bigramsAll <- aggregate(frequency ~ words, data = rbind(bigramsT1000, bigramsN1000, bigramsB1000), 
                        sum, na.rm = TRUE)
bigramsAll <- bigramsAll2[order(as.numeric(-bigramsAll2$frequency)),]
bigramsAll$words <- gsub("_", " ", bigramsAll$words); rownames(bigramsAll) <- NULL

## combine news, blogs and twitter trigrams
trigramsAll <- aggregate(frequency ~ words, data = rbind(trigramsT1000, trigramsN1000, trigramsB1000), 
                        sum, na.rm = TRUE)
trigramsAll <- trigramsAll[order(as.numeric(-trigramsAll$frequency)),]
trigramsAll$words <- gsub("_", " ", trigramsAll$words); rownames(trigramsAll) <- NULL

## write the biles out
write.csv(bigramsAll, file = "bigramsTop.csv")
write.csv(trigramsAll, file = "trigramsTop.csv")


