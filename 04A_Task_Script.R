#' ---
#' title: 'Task 04A: Fast Ngram Files'
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#+ setup, echo=FALSE
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
  library(ngram)
})

#' ## Load the Data
#+ DataLoading

#' English Repository Files
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  

#' Read the data files
blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file,  skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)

#' Read the data files into dataframes
blogs   <- data_frame(text = blogs)
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)

#' ## Sample the data
#+ DataSampling
set.seed(1001)
sample_pct <- 0.25

blogs_sample <- blogs %>%
  sample_n(., nrow(blogs)*sample_pct)
news_sample <- news %>%
  sample_n(., nrow(news)*sample_pct)
twitter_sample <- twitter %>%
  sample_n(., nrow(twitter)*sample_pct)

#' Create tidy repository
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)

#' Clean up
rm(list = c("blogs", "blogs_file", "blogs_sample","news", "news_file",     
            "news_sample", "sample_pct", "twitter","twitter_file", 
            "twitter_sample"))

#' ## Clean the data
#' Create filters: non-alphanumeric's, url's, repeated letters(+3x)
#+ Data Cleaning
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words,
#' and ngrams.
clean_sample <-  repo_sample %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

rm(list = c("repo_sample"))

#' ## Create all n-grams
#+ Ngrams 
#' Bigrams
bigram_repo <- clean_sample  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#' Trigrams
trigram_repo <- clean_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

#' Quadgrams
quadgram_repo <- clean_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)

#' ## Reduce n-grams files
#+ ReduceNgrams 
#' Bigrams
bigram_cover <- bigram_repo %>%
  count(bigram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("bigram_repo"))

#' Trigrams
trigram_cover <- trigram_repo %>%
  count(trigram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("trigram_repo"))

#' Quadgrams
quadgram_cover <- quadgram_repo %>%
  count(quadgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("quadgram_repo"))

#' ## What does the distribution on ngrams look like?
#+ DistyPlot
# disty <- data_frame(ngram = c(rep("bigrams",   nrow(bigram_cover)),
#                              rep("trigrams",  nrow(trigram_cover)),
#                              rep("quadgrams", nrow(quadgram_cover))), 
#                    number = c(bigram_cover$n, trigram_cover$n, quadgram_cover$n))
# disty
# disty$ngram <- as.factor(disty$ngram)
# ggplot(data = disty, aes(y = number, x = ngram)) + geom_boxplot() + scale_y_log10()

#' ## Separate words
#+ NgramWords 
bi_words <- bigram_cover %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bi_words

tri_words <- trigram_cover %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
tri_words

quad_words <- quadgram_cover %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quad_words

#' Save separated words for prediction
saveRDS(bi_words, "./clean_repos/bi_words_fast_small.rds")
saveRDS(tri_words, "./clean_repos/tri_words_fast_small.rds")
saveRDS(quad_words, "./clean_repos/quad_words_fast_small.rds")

#' Clear workspace, time load
rm(list= ls())

go <- Sys.time()
bi_words <- readRDS("./clean_repos/bi_words_fast_small.rds")
tri_words  <- readRDS("./clean_repos/tri_words_fast_small.rds")
quad_words <- readRDS("./clean_repos/quad_words_fast_small.rds")

stop <- Sys.time()
(how_long <- stop - go)

#' end

