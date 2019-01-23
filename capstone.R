
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

list.files("final")
list.files("final/en_US")
list.files("final/de_DE")
list.files("final/fi_FI")
list.files("final/ru_RU")

## read the blogs and twitter files with readLines function
blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8")
twitter <- readLines("./final/en_US/en_US.twitter.txt", warn = FALSE, encoding="UTF-8")
news <- readLines("./final/en_US/en_US.news.txt", warn = FALSE, encoding="UTF-8")

## need to know the en_US.blogs.txt file size
file.info("./final/en_US/en_US.blogs.txt")
USblogsSize <- (file.info("./final/en_US/en_US.blogs.txt"))$size / 1024 ^ 2
UStwitterSize <- (file.info("./final/en_US/en_US.twitter.txt"))$size / 1024 ^ 2
USnewsSize <- (file.info("./final/en_US/en_US.news.txt"))$size / 1024 ^ 2

USblogsSize
UStwitterSize
USnewsSize

## basic summaries
str(news); summary(news)
str(blogs); summary(blogs)
str(twitter); summary(twitter)

## nchar shows the lenght of each row; max picks the biggest value of those
longrowNews <- max(nchar(news)); longrowNews
longrowBlogs <- max(nchar(blogs)); longrowBlogs
longrowTwitter <- max(nchar(twitter)); longrowTwitter

longrowAll <- c(longrowNews, longrowBlogs, longrowTwitter)
max(longrowAll) ## longest row in the datasets

## grep shows rows that fill the rule, length counts them. Code \\<love\\> would cut the word but it gives wrong answer
love2 <- length(grep("love", twitter))
hate2 <- length(grep("hate", twitter))
round(love2 / hate2)

## grep gives the row number of the rule, [] subsets it
grep("biostats", twitter)
twitter[grep("biostats", twitter)]

length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))

