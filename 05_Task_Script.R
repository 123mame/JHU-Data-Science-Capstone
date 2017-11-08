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
bigram <- function(input_words){
                    num <- length(input_words)
                    filter(bi_words, 
                          word1==input_words[num]) %>% 
                    top_n(1, n) %>%
                    filter(row_number() == 1L) %>%
                    select(num_range("word", 2)) %>%
                    as.character() -> out
                    ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
                    num <- length(input_words)
                    filter(tri_words, 
                            word1==input_words[num-1], 
                            word2==input_words[num])  %>% 
                    top_n(1, n) %>%
                    filter(row_number() == 1L) %>%
                    select(num_range("word", 3)) %>%
                    as.character() -> out
                    ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
                    num <- length(input_words)
                    filter(quad_words, 
                            word1==input_words[num-2], 
                            word2==input_words[num-1], 
                            word3==input_words[num])  %>% 
                    top_n(1, n) %>%
                    filter(row_number() == 1L) %>%
                    select(num_range("word", 4)) %>%
                    as.character() -> out
                    ifelse(out=="character(0)", trigram(input_words), return(out))
}


#' User Input
input <- data_frame(text = c("Heck of A Fine"))

#' Clean the Input
input_count <- str_count(input, boundary("word"))
input_words <- unlist(str_split(input, boundary("word")))
input_words <- tolower(input_words)


#' Call the matching functions
y <- ifelse(input_count == 1, bigram(input_words), 
  ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))

#' Output
paste(input, y, sep = " ")
