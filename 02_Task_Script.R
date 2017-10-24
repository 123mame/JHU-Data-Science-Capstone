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
#' After exploring the entire data set, we then reduce the data based on frequency.
#' 
start <- Sys.time()

#+ startup, echo = FALSE 
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
  library(ngram)
})

#' ## 2. Data loading and cleaning 
#+ DataLoading

#' English Repository Files
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  

#' File Sizes (Mb)
blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)

#' Read the data files
blogs   <- read_lines(blogs_file)
news    <- read_lines(news_file)
twitter <- read_lines(twitter_file)

#' Number of Lines per file
blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines

#' Distibution of characters per line, by file
blogs_nchar   <- nchar(blogs)
news_nchar    <- nchar(news)
twitter_nchar <- nchar(twitter)

boxplot(blogs_nchar, news_nchar, twitter_nchar, log = "y",
        names = c("blogs", "news", "twitter"),
        ylab = "log(Number of Characters)", xlab = "File Name") 
title("Comparing Distributions of Chracters per Line")

#' Total characters per file
blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

#' Total words per file
blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(news, sep = " ")

#' Create summary of repo stats
repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))
repo_summary <- repo_summary %>% mutate(pct_n_char = round(n_char/sum(n_char), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
repo_summary <- repo_summary %>% mutate(pct_words = round(n_words/sum(n_words), 2))
kable(repo_summary)

#' Read the data files into dataframes
blogs_df   <- data_frame(text = readLines(blogs_file, skipNul = TRUE, warn = FALSE))
news_df    <- data_frame(text = readLines(news_file,  skipNul = TRUE))
twitter_df <- data_frame(text = readLines(twitter_file, skipNul = TRUE, warn = FALSE))


#' Create filters: stopwords, profanity, non-alphanumeric's, url's, repeated letters(+3x)
#+ DataCleaning
data("stop_words")
swear_words <- read_delim("./data/final/en_US/en_US.swearWords.csv", delim = "\n", col_names = FALSE)
swear_words <- unnest_tokens(swear_words, word, X1)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Clean dataframes from each souce. Cleaning is separted from tidying so `unnest_tokens` function can be used for words,
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

clean_twitter <- twitter %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

#' Clean up
rm(blogs, news, twitter, replace_reg, replace_url, replace_aaa)
x <- gc()
  
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

#' Save tidy repository; note repository size (MB)
saveRDS(tidy_repo, "./data/final/en_US/tidy_repo.rds")
(tidy_repo_size <- file.size("./data/final/en_US/tidy_repo.rds") / (2^20))

#' Save intermediate files for n-grams
saveRDS(clean_blogs, "./data/final/en_US/clean_blogs.rds")
saveRDS(clean_news, "./data/final/en_US/clean_news.rds")
saveRDS(clean_twitter, "./data/final/en_US/clean_twitter.rds")

#' ## 3. Most frequent words and word distributions

#' Word counts: Number of unique words in repo
(repo_count <- tidy_repo %>%
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

#' ## 4. Word distributions
#' Most frequent words by proportion, with source
freq <- tidy_repo %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))
kable(head(freq, 10))

#' Words above cutoff proportion: number of unique words
cutoff <- 0.0001
(small_repo_count <- freq %>%
    filter(proportion > cutoff) %>%
    summarise(keys = n_distinct(word)))

#' Word distribution by count
cover_90 %>%
  #count(word, sort = TRUE) %>%
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

#' Word cloud
cover_90 %>%
  #count(word) %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))

#' ## 5. Bigrams  
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

#' Number of bigrams to attain 90% coverage of all bigrams in repo
bigram_cover_90 <- bigram_repo %>%
  count(bigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(bigram_cover_90)

#' Bigram distribution
bigram_cover_90 %>%
  #count(bigram, sort = TRUE) %>%
  filter(n > 50000) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

##############
#' ## 6. Trigrams  
#' Create Trigrams by source using `unnest_tokens`
#+ trigrams
x <- gc()

set.seed(1001)

blogs_trigrams <- clean_blogs  %>%
  sample_n(., nrow(clean_blogs)*0.10) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

news_trigrams <- clean_news  %>%
  sample_n(., nrow(clean_news)*0.10) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

twitter_trigrams <- clean_twitter  %>%
  sample_n(., nrow(clean_twitter)*0.10) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

#' Create tidy trigram repository
trigram_repo <- bind_rows(mutate(blogs_trigrams, source = "blogs"),
                         mutate(news_trigrams,  source = "news"),
                         mutate(twitter_trigrams, source = "twitter"))
trigram_repo$source <- as.factor(trigram_repo$source)

#' Number of trigrams to attain 90% coverage of all trigrams in repo
trigram_cover_90 <- trigram_repo %>%
  count(trigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(trigram_cover_90)

#' trigram distribution
trigram_cover_90 %>%
  #count(trigram, sort = TRUE) %>%
  filter(n > 750) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#' ## 7. Fourgrams  
#' Create Fourgrams by source using `unnest_tokens`
#+ fourgrams
x <- gc()

set.seed(1001)

blogs_fourgrams <- clean_blogs  %>%
  sample_n(., nrow(clean_blogs)*0.005) %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4)

news_fourgrams <- clean_news  %>%
  sample_n(., nrow(clean_news)*0.05) %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4)

twitter_fourgrams <- clean_twitter  %>%
  sample_n(., nrow(clean_twitter)*0.05) %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4)

#' Create tidy fourgram repository
fourgram_repo <- bind_rows(mutate(blogs_fourgrams, source = "blogs"),
                          mutate(news_fourgrams,  source = "news"),
                          mutate(twitter_fourgrams, source = "twitter"))
fourgram_repo$source <- as.factor(fourgram_repo$source)

#' Number of fourgrams to attain 90% coverage of all fourgrams in repo
fourgram_cover_90 <- fourgram_repo %>%
  count(fourgram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(fourgram_cover_90)

#' Fourgram distribution
fourgram_cover_90 %>%
  #count(trigram, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(fourgram = reorder(fourgram, n)) %>%
  ggplot(aes(fourgram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

fourgrams_separated <- fourgram_cover_90 %>%
  separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ")
fourgrams_separated


end <- Sys.time()

(run_time <- end - start)
###############

#' -------------
#'  
#' #### Session info:
#+ show-sessionInfo
sessionInfo()       
