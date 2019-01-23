
setwd("~/Data_Science_Specialization/capstone_project")
library(stringi)

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


wordsTwitter <- stri_stats_latex(twitter)[4]
charTwitter <- sum(nchar(twitter))
longrowTwitter <- max(nchar(twitter))
sizeTwitter <- (file.info("./final/en_US/en_US.twitter.txt"))$size / 1024 ^ 2

wordsBlogs <- stri_stats_latex(blogs)[4]
charBlogs <- sum(nchar(blogs))
longrowBlogs <- max(nchar(blogs))
sizeBlog <- (file.info("./final/en_US/en_US.blogs.txt"))$size / 1024 ^ 2

wordsNews <-stri_stats_latex(news)[4]
charNews<-sum(nchar(news))
longrowNews <- max(nchar(news))
sizeNews <- (file.info("./final/en_US/en_US.news.txt"))$size / 1024 ^ 2

data.frame("source" = c("twitter", "blogs", "news"),
           "lines" = c(length(twitter),length(blogs), length(news)),
           "words" = c(wordsTwitter, wordsBlogs,wordsNews),
           "characters"=c(charTwitter,charBlogs, charNews),
           "longest line"=c(longrowTwitter,longrowBlogs,longrowNews),
           "file size"=c(sizeTwitter, sizeBlog, sizeNews))

set.seed(112) # set seed to be able to reproduce the sampling
library(caret)
library(tm) # text mining library

## let's do a sample data set which includes 5 percent of each data set
sampleTwitter <- sample(twitter, length(twitter) * 0.05)
sampleBlogs <- sample(blogs, length(blogs) * 0.05)
sampleNews <- sample(news, length(news) * 0.05)

sampleData <- c(sampleTwitter, sampleBlogs, sampleNews)
sampleData <- removePunctuation(sampleData) # remove punctuation
summary(sampleData)

## as I did not get Rweka working, I had to develop something else. I found quanteda which can do both corpus and ngrams.
library(quanteda)
corpus <- corpus(sampleData)

unigrams<-dfm(corpus,
              ngrams = 1)
headUnigrams <- data.frame(topfeatures(unigrams, 20)); names(headUnigrams) <- c("frequency"); # make a df
headUnigrams$words <- rownames(headUnigrams); # make rown names as a column
headUnigrams$words <- factor(headUnigrams$words, levels = headUnigrams$words[order(-headUnigrams$frequency)]) # sort

gg <- ggplot(headUnigrams, aes(headUnigrams$words, headUnigrams$frequency)) # make graph
gg + geom_bar(stat='identity', col="black", fill="orange")+labs(title="Unigram words", x="",y="Frequency") +
        theme(axis.text.x = element_text(angle = 90))

## do the same for bi, tri and 4-grams
bigrams <- dfm(corpus,
             ngrams = 2,
             concatenator=" ")
headBigrams <- data.frame(topfeatures(bigrams, 20)); names(headBigrams) <- c("frequency"); # make a df
headBigrams$words <- rownames(headBigrams); # make rown names as a column
headBigrams$words <- factor(headBigrams$words, levels = headBigrams$words[order(-headBigrams$frequency)]) # sort

gg <- ggplot(headBigrams, aes(headBigrams$words, headBigrams$frequency)) # make graph
gg + geom_bar(stat='identity', col="black", fill="red")+labs(title="Bigram words", x="",y="Frequency") +
        theme(axis.text.x = element_text(angle = 90))

trigrams <- dfm(corpus,
              ngrams = 3,
              concatenator=" ")
headTrigrams <- data.frame(topfeatures(trigrams, 20)); names(headTrigrams) <- c("frequency"); # make a df
headTrigrams$words <- rownames(headTrigrams); # make rown names as a column
headTrigrams$words <- factor(headTrigrams$words, levels = headTrigrams$words[order(-headTrigrams$frequency)]) # sort

gg <- ggplot(headTrigrams, aes(headTrigrams$words, headTrigrams$frequency)) # make graph
gg + geom_bar(stat='identity', col="black", fill="green")+labs(title="Trigram words", x="",y="Frequency") +
theme(axis.text.x = element_text(angle = 90))

fourgram <- dfm(corpus,
                ngrams = 4,
                concatenator=" ")
headFourgram <- data.frame(topfeatures(fourgram, 20)); names(headFourgram) <- c("frequency"); # make a df
headFourgram$words <- rownames(headFourgram); # make rown names as a column
headFourgram$words <- factor(headFourgram$words, levels = headFourgram$words[order(-headFourgram$frequency)]) # sort

gg <- ggplot(headFourgram, aes(headFourgram$words, headFourgram$frequency)) # make graph
gg + geom_bar(stat='identity', col="black", fill="blue")+labs(title="4-gram words", x="",y="Frequency") +
        theme(axis.text.x = element_text(angle = 90))


##########################

bigramsAll <- data.frame(topfeatures(bigrams, 1000))
names(bigramsAll) <- c("frequency"); # make a df
bigramsAll$words <- rownames(bigramsAll); # make rown names as a column
bigramsAll$words <- factor(bigramsAll$words, levels = bigramsAll$words[order(-bigramsAll$frequency)])
rownames(bigramsAll) <- NULL


trigramsAll <- data.frame(topfeatures(trigrams, 1000))
names(trigramsAll) <- c("frequency"); # make a df
trigramsAll$words <- rownames(trigramsAll); # make rown names as a column
trigramsAll$words <- factor(trigramsAll$words, levels = trigramsAll$words[order(-trigramsAll$frequency)])
rownames(trigramsAll) <- NULL


fourgramsAll <- data.frame(topfeatures(fourgram, 1000))
names(fourgramsAll) <- c("frequency"); # make a df
fourgramsAll$words <- rownames(fourgramsAll); # make rown names as a column
fourgramsAll$words <- factor(fourgramsAll$words, levels = fourgramsAll$words[order(-fourgramsAll$frequency)])
rownames(fourgramsAll) <- NULL


