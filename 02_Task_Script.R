#' ---
#' title: "Task 2: Exploratory Data Analysis"
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' ## 1. Introduction
#' This script uses the tidy data principles applied to text mining, as outlined in
#' [Text Mining with R: A Tidy Approach](http://tidytextmining.com/).  
#' 
#' Using this approach, we are able to use the **entire data set** as opposed
#' to data sampling approach required by the memory constraints of the `tm` package.
#' 
#+ startup, echo = FALSE 
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
})

#' ## 1. Data loading and cleaning 

#' English Repository Files
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  

#' Read the data files
blogs   <- readLines(blogs_file,   skipNul = TRUE)
news    <- readLines(news_file,    skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE) 

#' Create filters: stopwords, profanity, non-alphanumeric characters, url's, 
#' repeated letters
#+ chunkmessage, warnings = FALSE, messages = FALSE
data("stop_words")
swear_words <- read.csv("./data/final/en_US/en_US.swearWords.csv", header = FALSE)
swear_words <- gather(swear_words) %>% transmute(word = value)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Create clean & tidy dataframes for each source and a clean & tidy repository
clean_blogs <- data_frame(text = blogs) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT"))


#' ############  
#' SPLIT CLEANING FROM TIDYING, this allows use of unnest_tokens for ngrams
#' READ MORE BEFORE DOING MORE

tidy_blogs <- clean_blogs  %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)

## ngrams
tidy_blogs_bigrams <- clean_blogs  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
tidy_blogs_bigrams

################

tidy_news <- data_frame(text = news) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT")) %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)

tidy_twitter <- data_frame(text = twitter) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT")) %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)

# Tidy repository with source, save to file  
tidy_repo <- bind_rows(mutate(tidy_blogs, source = "blogs"),
                       mutate(tidy_news,  source = "news"),
                       mutate(tidy_twitter, source = "twitter")) 
tidy_repo$source <- as.factor(tidy_repo$source)
saveRDS(tidy_repo, "./data/final/en_US/tidy_repo.rds")

#' ## 2. Most frequent words and word distributions

freq <- tidy_repo %>%
  #mutate(word = str_extract(word, "[a-z']+")) %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  #select(-n) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))

#' Most frequent words
kable(head(freq, 10))

#' Least frequent words
freq_low <- freq %>% 
  arrange(proportion, n)
kable(head(freq_low, 10)) 
  
#' Word clouds
tidy_blogs %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
tidy_blogs %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE)) 

#' Word distribution
tidy_repo %>%
  count(word, sort = TRUE) %>%
  filter(n > 40000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#' Word distribution by source
freq %>%
  group_by(source) %>%
  filter(proportion > 0.0025) %>% 
  mutate(word = reorder(word, proportion)) %>% 
  ggplot(aes(word, proportion)) +
  geom_col() + 
  xlab(NULL) + 
  coord_flip() +
  facet_wrap(~source) 

#' 


#' -------------
#'  
#' #### Session info:
#+ show-sessionInfo
sessionInfo()       
