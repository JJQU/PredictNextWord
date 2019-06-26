suppressWarnings(library(shiny))
suppressWarnings(library(stringi))
suppressWarnings(library(stringr))
suppressWarnings(library(tm))
suppressWarnings(library(qdap))
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))
suppressWarnings(library(ngram))


load(file = "~/Shiny/PredictNextWord1/model2.rda")
source('~/Shiny/PredictNextWord1/NgramModeling.R')
source('~/Shiny/PredictNextWord1/Preprocess.R')


shinyServer(function(input, output) {
  
  predict <- function(inputStr) {
    ## Check for an empty string
    if (inputStr == "") {
      return("<UNK>")
    }
    inputCorpus <- SimpleCorpus(VectorSource(inputStr))
    inputCorpus <- preprocessData(inputCorpus)
    
    cleanStr <- concatenate(inputCorpus$content)[[1]]
    if (cleanStr == " ") {
      return("<UNK>")
    }
    
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
    result});
  
  output$inputWords <- renderText({
    input$inputText})  
  
}
) 


