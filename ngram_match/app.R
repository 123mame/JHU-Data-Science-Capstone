#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
suppressPackageStartupMessages({
   library(tidyverse)
   library(stringr)
 })

#' Source ngram matching function
source("ngram.R")

#' Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Text Prediction Model"),
   p("This app that takes an input phrase (multiple words) in a text box and outputs a prediction of the next word."),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h3("Instructions"), 
        h5("1. Enter a word or words in the text box."),
        h5("2. The prediction prints below it in blue."),
        h5("3. No need to hit enter of submit."),
        h5("4. A question mark means no prediction, typically do to input mis-spelling"),
        br(),
        a("Source Code", href = "https://github.com/mark-blackmore/JHU-Data-Science-Capstone/tree/master/ngram_match")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textInput("user_input", h3("Your Input:"), 
                  value = "Your words"),
        h3("Predicted Next Word:"),
        h4(em(span(textOutput("ngram_output"), style="color:blue")))
        
        )   
   )
)

#' Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$ngram_output <- renderText({
      ngrams(input$user_input)
  })
  
}
  
#' Run the application 
shinyApp(ui = ui, server = server)

