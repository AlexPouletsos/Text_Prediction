library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        titlePanel("Text Predictor"),
        
        textAreaInput("wordIn", "Enter Text"),
        
        fluidRow(
                column(2, actionButton("Button1", textOutput("prediction1"))),
                column(2, actionButton("Button2", textOutput("prediction2"))),
                column(2, actionButton("Button3", textOutput("prediction3"))))
        )
)

