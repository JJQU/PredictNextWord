
library(shiny)
library(tm)
library(stringr)
library(qdap)
library(dplyr)

getwd()
#setwd("C:/Users/JJQ/Documents/Shiny/PredictionTest/")

load(file = "model1.rda")

shinyServer(function(input, output) {
  
  predict <- function(input) {
    ## Check for an empty string
    if (input == "") {
      return("<UNK>")
    }
    
    ## Convert the input string to corpus
    inputCorpus <- SimpleCorpus(VectorSource(input))
    
    removedStopwords <- FALSE
    
    ## Clean the training, validate, and test corpuses
    
    ToSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    inputCorpus <- tm_map(inputCorpus, ToSpace, "/")
    inputCorpus <- tm_map(inputCorpus, ToSpace, "@")
    inputCorpus <- tm_map(inputCorpus, ToSpace, "\\|")
    inputCorpus <- tm_map(inputCorpus, ToSpace, "\\b[A-z]\\b{1}")
    inputCorpus <- tm_map(inputCorpus, ToSpace, "#")
    inputCorpus <- tm_map(inputCorpus, ToSpace, "http:[[:alnum:]]*")
    inputCorpus <- tm_map(inputCorpus, content_transformer(tolower))
    inputCorpus <- tm_map(inputCorpus, removeNumbers)
    inputCorpus <- tm_map(inputCorpus, removePunctuation)
    inputCorpus <- tm_map(inputCorpus, stripWhitespace)
    
    ## Convert corpus to a string
    cleanStr <- concatenate(inputCorpus$content)[[1]]
    if (cleanStr == " ") {
      return("<UNK>")
    }
    
    ## Split the clean string into words
    inputWords <- str_split(cleanStr, boundary("word"))[[1]]
    
    if (length(inputWords) > 1) {
      testWords <- tail(inputWords, 2)
    } else if (length(inputWords) > 0) {
      testWords <- c("<UNK>", tail(inputWords, 1))
    } else {
      return("<UNK>")
    }
    
    predictions <- trimodel(testWords, coef2, 
                            ugramPzero.f8, 
                            bgramPzero.f2.f8, 
                            tgramPzero.f2.f8, 1)
    
    cnames <- colnames(predictions)
    if ("word3" %in% cnames) {
      nextWords <- predictions$word3
    } else if ("word2" %in% cnames) {
      nextWords <- predictions$word2
    } else {
      nextWords <- predictions$word1
    }
    return(nextWords)
  }
  
  
  output$NextWord <- renderPrint({
      result <- predict(input$inputText)
      result
    });
  
  output$inputWords <- renderText({
      input$inputText})  
  
})