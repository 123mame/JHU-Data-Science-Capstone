#' ---
#' title: "Task 2: Exploratory Data Analysis"
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
library(tidytext)
library(tidyverse)
library(stringr)
library(knitr)
library(wordcloud)
data("stop_words")

#' English Repository Files
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  

#' Read the data files
blogs   <- readLines(blogs_file,   skipNul = TRUE)
news    <- readLines(news_file,    skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE) 

profanity <- read.table("./data/final/en_US/profanity.txt", header = FALSE, sep ="\n")
profanity <- data.frame(text = profanity)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"


tidy_blogs <- data_frame(text = blogs) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = iconv(text, "ASCII//TRANSLIT")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_news <- data_frame(text = news) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = iconv(text, "ASCII//TRANSLIT")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_twitter <- data_frame(text = twitter) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = iconv(text, "ASCII//TRANSLIT")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 
  
tidy_repo <- bind_rows(mutate(tidy_blogs, source = "blogs"),
                       mutate(tidy_news,  source = "news"),
                       mutate(tidy_twitter, source = "twitter")) 

freq <- tidy_repo %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  #select(-n) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))

kable(head(freq, 10))

freq_low <- freq %>% 
  arrange(proportion, n)

kable(head(freq_low, 10)) 
  
# Word cloud
tidy_blogs %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, random.order = FALSE))

tidy_blogs %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq = 5000, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE) ) 
       