#' ---
#' title: 'Task 05: Creative Exploration'
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#+ setup, echo=FALSE
go <- Sys.time()
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

#+ data, echo=FALSE
bi_words <- readRDS("./clean_repos/bi_words.rds")
tri_words  <- readRDS("./clean_repos/tri_words.rds")
quad_words <- readRDS("./clean_repos/quad_words.rds")

#+ functions
bigram <- function(input_words, input_count){
                    filter(bi_words, 
                          word1==input_words[input_count]) %>% 
                    top_n(1, n) %>%
                    filter(row_number() == 1L) %>%
                    select(num_range("word", input_count+1)) %>%
                    as.character()
}

trigram <- function(input_words, input_count){
                    filter(tri_words, 
                            word1==input_words[input_count-1], 
                            word2==input_words[input_count])  %>% 
                    top_n(1, n) %>%
                    filter(row_number() == 1L) %>%
                    select(num_range("word", input_count+1)) %>%
                    as.character()
}

quadgram <- function(input_words, input_count){
                    filter(quad_words, 
                            word1==input_words[input_count-2], 
                            word2==input_words[input_count-1], 
                            word3==input_words[input_count])  %>% 
                    top_n(1, n) %>%
                    filter(row_number() == 1L) %>%
                    select(num_range("word", input_count+1)) %>%
                    as.character()
}


#' User Input
input <- data_frame(text = c("in case of"))

#' Logic to Predict
input_count <- str_count(input, boundary("word"))
input_words <- unlist(str_split(input, boundary("word")))


if (input_count == 1) {
    bigram(input_words, input_count)
  
} else {
  
  if (input_count == 2){
      trigram(input_words, input_count)
    
  } else {    
      quadgram(input_words, input_count)
  }
}

#' Program output
