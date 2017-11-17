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
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h3("Instructions"), 
        p("What happens if I do this. Will the text wrap?  1. Can I enumerate?"),
        helpText("This app predicts."),
        h5("1. Enter some words or a phrase in the text box."),
        h5("2. ")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textInput("user_input", h3("Your Input:"), 
                  value = "Your words"),
        h3("Suggested Phrase:"),
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

