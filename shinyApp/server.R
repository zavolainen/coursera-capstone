# Author: Jani Savolainen
# Project: Coursera Data Science Capstone Project
# Date: 13/02/2019
#

require(shiny)
require(data.table)
require(quanteda)

shinyServer(function(input, output, session) {
        library(quanteda)
        load("grams.RData")
        # output value passed to ui.R
        output$finalPrediction <- renderText({
                if (input$userInput == ""){
                        return("")
                }else {
#                        return(paste("Next word prediction: ", paste(nextWordPrediction(input$userInput), 
#                                                                     collapse =" | "), sep = ""))
                        return(paste("Next word prediction: \n", paste(nextWordPrediction(input$userInput), 
                                                                     collapse ="\n"), sep = ""))
                }
        })

        # function to return the most probable next words based on two previous words
        twoWords <- function(w1, w2, n = 5) {
                pwords <- wordsTri[.(w1, w2)][order(-Prob)]
                if (any(is.na(pwords)))
                        return(oneWords(w2, n))
                if (nrow(pwords) > n)
                        return(pwords[1:n, word_3])
                count <- nrow(pwords)
                bwords <- oneWords(w2, n)[1:(n - count)]
                return(c(pwords[, word_3], bwords))
        }
        
        # function to return the most probable next words based on the previous word
        oneWords <- function(w1, n = 5) {
                pwords <- wordsBi[w1][order(-Prob)]
                if (any(is.na(pwords)))
                        return(uniWords(n))
                if (nrow(pwords) > n)
                        return(pwords[1:n, word_2])
                count <- nrow(pwords)
                unWords <- uniWords(n)[1:(n - count)]
                return(c(pwords[, word_2], unWords))
        }
        
        # function to return random words from unigrams - used if no matches on twoWords or oneWords
        uniWords <- function(n = 5) {  
                return(sample(wordsUni[, word_1], size = n))
        }

        # 
        getPreviousWords <- function(str) {
                words <- tokens(x = char_tolower(str))
                char_wordstem(rev(rev(words[[1]])[1:2]), language = "english")
        }
                
        nextWordPrediction <- function(str) {
                words <- getPreviousWords(str)
                pwords <- twoWords(words[1], words[2], 5)
                pwords
        }
        

        
})