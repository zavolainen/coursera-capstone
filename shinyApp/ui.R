# Author: Jani Savolainen
# Project: Coursera Data Science Capstone Project
# Date: 13/02/2019
#
#
library(shiny)

shinyUI(fluidPage(
        titlePanel("Next Word Predictor"),
        
        sidebarLayout(
                position = "right",
                sidebarPanel(
                        p("This is a Shiny app which has a algorithm predicting the next possible 
                        words based on the previous words. App is created as a capstone project in
                        Coursera Data Science Specialization."), 
                        p("It is created by", 
                          a("Jani Savolainen", 
                            href = "https://github.com/zavolainen/"), "and influenced by ",
                          a("Thiloshon Nagarajah's", 
                            href = "https://thiloshon.wordpress.com/2018/03/11/build-your-own-word-sentence-prediction-application-part-02/"),
                        "tutorial."), 
                        p("Algorith is based on the data from SwiftKey whose product is a virtual keyboard app.",
                          a("Data", href = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"), 
                          "is hosted by Coursera.")
                        ),
                
                
                
                # Show a plot of the generated distribution
                mainPanel(
                        align="center",
                        textAreaInput(
                                "userInput",
                                "Type a word or sentence and get a prediction of what next word might be",
                                value = "",
                                placeholder = "Start typing..."
                                ),
                        
                        verbatimTextOutput("finalPrediction")
                )
        )
))
