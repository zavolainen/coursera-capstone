set.seed(112) # set seed to be able to reproduce the sampling
library(quanteda)
library(data.table)

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

## do a sample set - twitter 30%, blogs and news 50%
sampleHolderTwitter <- sample(length(twitter), length(twitter) * 0.2)
sampleHolderBlog <- sample(length(blogs), length(blogs) * 0.4)
sampleHolderNews <- sample(length(news), length(news) * 0.4)

twitterSample <- twitter[sampleHolderTwitter]
blogsSample <- blogs[sampleHolderBlog]
newsSample <- news[sampleHolderNews]

allSample <- c(twitterSample, blogsSample, newsSample)
allCorpus <- corpus(allSample)

# the puncuations and numbers in the texts were removed as there is no need to predict punctations or numbers
allTokens <- tokens(
        x = tolower(allCorpus),
        remove_punct = TRUE,
        remove_twitter = TRUE,
        remove_numbers = TRUE,
        remove_hyphens = TRUE,
        remove_symbols = TRUE,
        remove_url = TRUE
)

wordStem <- tokens_wordstem(allTokens, language = "english")
bigrams <- tokens_ngrams(wordStem, n = 2)
trigrams <- tokens_ngrams(wordStem, n = 3)

uniDFM <- dfm(wordStem)
biDFM <- dfm(bigrams)
triDFM <- dfm(trigrams)

uniDFM <- dfm_trim(uniDFM, 3)
biDFM <- dfm_trim(biDFM, 3)
triDFM <- dfm_trim(triDFM, 3)

# Create named vectors with counts of words 
sumsUnigram <- colSums(uniDFM)
sumsBigram <- colSums(biDFM)
sumsTrigram <- colSums(triDFM)

# Create data tables with individual words as columns
wordsUni <- data.table(word_1 = names(sumsUnigram), count = sumsUnigram)
wordsUni <- wordsUni[order(wordsUni$count, decreasing = T)]

wordsBi <- data.table(
        word_1 = sapply(strsplit(names(sumsBigram), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sumsBigram), "_", fixed = TRUE), '[[', 2),
        count = sumsBigram)
wordsBi <- wordsBi[order(wordsBi$count, decreasing = T)]

wordsTri <- data.table(
        word_1 = sapply(strsplit(names(sumsTrigram), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sumsTrigram), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sumsTrigram), "_", fixed = TRUE), '[[', 3),
        count = sumsTrigram)
wordsTri <- wordsTri[order(wordsTri$count, decreasing = T)]

setkey(wordsUni, word_1)
setkey(wordsBi, word_1, word_2)
setkey(wordsTri, word_1, word_2, word_3)




#Finding bigram / trigram Probability (thanks to Thiloshon Nagarajah https://github.com/thiloshon)
discountValue <- 0.75

numBigrams <- nrow(wordsBi[by = .(word_1, word_2)])
ckn <- wordsBi[, .(Prob = ((.N) / numBigrams)), by = word_2]
setkey(ckn, word_2)

# Assigning the probabilities as second word of bigram, to unigrams
wordsUni[, Prob := ckn[word_1, Prob]]
wordsUni <- wordsUni[!is.na(wordsUni$Prob)]

# Finding number of times word 1 occurred as word 1 of bigrams
n1wi <- wordsBi[, .(N = .N), by = word_1]
setkey(n1wi, word_1)
wordsBi[, Cn1 := wordsUni[word_1, count]]

# Kneser Kney Algorithm
wordsBi[, Prob := ((count - discountValue) / Cn1 + discountValue / Cn1 * n1wi[word_1, N] * wordsUni[word_2, Prob])]

# Trigram probability
# Finding count of word1-word2 combination in bigram 
wordsTri[, Cn2 := wordsBi[.(word_1, word_2), count]]

# Finding count of word1-word2 combination in trigram
n1w12 <- wordsTri[, .N, by = .(word_1, word_2)]
setkey(n1w12, word_1, word_2)

# Kneser Kney Algorithm
wordsTri[, Prob := (count - discountValue) / Cn2 + discountValue / Cn2 * n1w12[.(word_1, word_2), N] *
                  wordsBi[.(word_1, word_2), Prob]]

######## End of Finding Tri-Gram Probability #################

# Finding the most frequently used 50 unigrmas
wordsUni <- wordsUni[order(-Prob)][1:50]

# function to return highly probable previous word given two successive words
triWords <- function(w1, w2, n = 5) {
        pwords <- wordsTri[.(w1, w2)][order(-Prob)]
        if (any(is.na(pwords)))
                return(biWords(w2, n))
        if (nrow(pwords) > n)
                return(pwords[1:n, word_3])
        count <- nrow(pwords)
        bwords <- biWords(w2, n)[1:(n - count)]
        return(c(pwords[, word_3], bwords))
}

# function to return highly probable previous word given a word
biWords <- function(w1, n = 5) {
        pwords <- wordsBi[w1][order(-Prob)]
        if (any(is.na(pwords)))
                return(uniWords(n))
        if (nrow(pwords) > n)
                return(pwords[1:n, word_2])
        count <- nrow(pwords)
        unWords <- uniWords(n)[1:(n - count)]
        return(c(pwords[, word_2], unWords))
}

# function to return random words from unigrams
uniWords <- function(n = 5) {  
        return(sample(wordsUni[, word_1], size = n))
}

# The prediction app
getWords <- function(str){
        tokens <- tokens(x = char_tolower(str))
        tokens <- char_wordstem(rev(rev(tokens[[1]])[1:2]), language = "english")
        
        words <- triWords(tokens[1], tokens[2], 5)
        chain_1 <- paste(tokens[1], tokens[2], words[1], sep = " ")
        
        print(words[1])
}

save(wordsUni, wordsBi, wordsTri, file = "grams.RData")
write.csv(wordsBi, file = "topBigrams.csv")
write.csv(wordsTri, file = "topTrigrams.csv")

