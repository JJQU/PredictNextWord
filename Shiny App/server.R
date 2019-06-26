suppressWarnings(library(shiny))
suppressWarnings(library(stringi))
suppressWarnings(library(stringr))
suppressWarnings(library(tm))
suppressWarnings(library(qdap))
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))
suppressWarnings(library(ngram))


#load(file = "~/Shiny/PredictNextWord1/model2.rda")

source('./Prediction.R')

shinyServer(function(input, output) {
  
  
  output$NextWord <- renderPrint({
    result <- predict2(input$inputText)
    result});
  
  output$inputWords <- renderText({
    input$inputText})  
  
}
) 


