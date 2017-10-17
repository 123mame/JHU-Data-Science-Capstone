library(tidytext)
library(tidyverse)
library(stringr)
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

tidy_blogs <- data_frame(text = blogs) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%

tidy_news <- data_frame(text = news) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_twitter <- data_frame(text = twitter) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

  
tidy_repo <- bind_rows(mutate(tidy_blogs, source = "blogs"),
                       mutate(tidy_news,  source = "news"),
                       mutate(tidy_twitter, source = "twitter")) 

frequncey <- tidy_repo %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  #select(-n) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))
frequncey
head(frequncey, 20)

# Word cloud
tidy_repo %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

