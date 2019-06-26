library(shiny)
library(tm)
library(stringr)



shinyUI(
  navbarPage("Next Word Prediction",
             tabPanel("Predict Word",
                      
                      sidebarLayout(
                        sidebarPanel(
                          helpText("This app is to provide an interactive way to predict next word."),
                          hr(),
                          textInput("inputText", "Enter",value = ""),
                          hr(),
                          hr(),
                          hr()
                        ),
                        mainPanel(
                          h2("Word Predicton Result"),
                          strong("Your Input"),
                          verbatimTextOutput("inputWords"),
                          hr(),
                          strong("Predicted Next Word"),
                          strong(code(textOutput("NextWord"))),
                          hr(),
                          hr()
                        )
                      )
             )
  )
)
