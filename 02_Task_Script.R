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
start <- Sys.time()

#+ startup, echo = FALSE 
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
})

#' ## 2. Data loading and cleaning 

#' English Repository Files
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  

#' Read the data files
blogs   <- data_frame(text = readLines(blogs_file,   skipNul = TRUE, warn = FALSE))
news    <- data_frame(text = readLines(news_file,    skipNul = TRUE, warn = FALSE))
twitter <- data_frame(text = readLines(twitter_file, skipNul = TRUE, warn = FALSE)) 

#' Create filters: stopwords, profanity, non-alphanumeric, url's, repeated letters
data("stop_words")
swear_words <- read_delim("./data/final/en_US/en_US.swearWords.csv", delim = "\n", col_names = FALSE)
swear_words <- unnest_tokens(swear_words, word, X1)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Clean dataframes for each souce. Cleaning is separted from tidying so `unnest_tokens` function can be used for words,
#' and ngrams.
clean_blogs <-  blogs %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

clean_news <-   news %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

clean_twitter <- twitter%>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

#' Clean up
rm(blogs, news, twitter)
gc()
  
#' Create tidy dataframes for each source
tidy_blogs <- clean_blogs %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)

tidy_news <- clean_news %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)

tidy_twitter <- clean_twitter %>%  
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)

#' Create tidy repository
tidy_repo <- bind_rows(mutate(tidy_blogs, source = "blogs"),
                       mutate(tidy_news,  source = "news"),
                       mutate(tidy_twitter, source = "twitter")) 
tidy_repo$source <- as.factor(tidy_repo$source)

#' Save tidy repository
saveRDS(tidy_repo, "./data/final/en_US/tidy_repo.rds")
(tidy_repo_size <- file.size("./data/final/en_US/tidy_repo.rds") / (2^20))

#' ## 3. Most frequent words and word distributions

#' Word counts: Number of unique words in repo
(repo_count <- tidy_repo %>%
    summarise(keys = n_distinct(word)))

# Words above cutoff proportion: number of unique words
cutoff <- 0.0001
(small_repo_count <- freq %>%
    filter(proportion > cutoff) %>%
    summarise(keys = n_distinct(word)))

#' Number of words to attain 50% and 90% coverage of all words in repo
cover_50 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.5)
nrow(cover_50)

cover_90 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(cover_90)

#' ## 4. Word distributions: using 90% coverage
#' Most frequent words
freq <- tidy_repo %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))
kable(head(freq, 10))

#' Word clouds
tidy_repo %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))

#' Word clouds by source
# Blogs
tidy_repo %>%
  filter(source == "blogs") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
# News
tidy_repo %>%
  filter(source == "news") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
# Twitter
tidy_repo %>%
  filter(source == "twitter") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))

#' Word distribution
tidy_repo %>%
  count(word, sort = TRUE) %>%
  filter(n > 35000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#' Word distribution by source
freq %>%
  filter(proportion > 0.002) %>% 
  mutate(word = reorder(word, proportion)) %>% 
  ggplot(aes(word, proportion)) +
  geom_col() + 
  xlab(NULL) + 
  coord_flip() +
  facet_grid(~source, scales = "free") 

################  
#' ## 4. Bigrams

#' Create bigrams by source using `unnest_tokens`
blogs_bigrams <- clean_blogs  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

news_bigrams <- clean_news  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

twitter_bigrams <- clean_twitter  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#' Create tidy bigram repository
bigram_repo <- bind_rows(mutate(blogs_bigrams, source = "blogs"),
                       mutate(news_bigrams,  source = "news"),
                       mutate(twitter_bigrams, source = "twitter"))
bigram_repo$source <- as.factor(bigram_repo$source)

#### WORKING

#' Bigram distribution
bigram_repo %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 50000) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# #' Bigram word cloud
# bigram_repo %>%
#   count(bigram) %>%
#   with(wordcloud(bigram, n, max.words = 25, 
#                  colors = brewer.pal(6, 'Dark2'), random.order = FALSE))

#' #' Number of words to attain 90% coverage of all words in repo
bigram_cover_90 <- bigram_repo %>%
  count(bigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(bigram_cover_90)


end <- Sys.time()

(run_time <- end - start)
################

#' -------------
#'  
#' #### Session info:
#+ show-sessionInfo
sessionInfo()       
