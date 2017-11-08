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


#' User Input
input <- data_frame(text = c("in case of"))

#' Logic to Predict
input_count <- str_count(input, boundary("word"))
input_words <- unlist(str_split(input, boundary("word")))


if (input_count == 1) {
  filter(bi_words, 
         word1==input_words[1]) %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", input_count+1)) %>%
    as.character()
} else {
  
  if (input_count == 2){
    filter(tri_words, 
           word1==input_words[1], 
           word2==input_words[2])  %>% 
      top_n(1, n) %>%
      filter(row_number() == 1L) %>%
      select(num_range("word", input_count+1)) %>%
      as.character()
    
  } else {    
    filter(quad_words, 
           word1==input_words[input_count-2], 
           word2==input_words[input_count-1], 
           word3==input_words[input_count])  %>% 
      top_n(1, n) %>%
      filter(row_number() == 1L) %>%
      select(num_range("word", input_count+1)) %>%
      as.character()
  }
}


# Simple Logic Test; What if no match
filter(quad_words,
       word1==input_words[input_count-2],
       word2==input_words[input_count-1],
       word3==input_words[input_count]) %>%
  top_n(1, n) %>%
  filter(row_number() == 1L) %>%
  select(num_range("word", input_count+1)) %>%
  as.character()

#' Program output
