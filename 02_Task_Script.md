Task 2: Exploratory Data Analysis
================
Mark Blackmore
2017-10-19

1. Introduction
---------------

This script uses the tidy data principles applied to text mining, as outlined in [Text Mining with R: A Tidy Approach](http://tidytextmining.com/).

Using this approach, we are able to use the **entire data set** as opposed to data sampling approach required by the memory constraints of the `tm` package.

``` r
start <- Sys.time()
```

1. Data loading and cleaning
----------------------------

English Repository Files

``` r
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  
```

Read the data files

``` r
blogs   <- data_frame(text = readLines(blogs_file,   skipNul = TRUE, warn = FALSE))
news    <- data_frame(text = readLines(news_file,    skipNul = TRUE, warn = FALSE))
twitter <- data_frame(text = readLines(twitter_file, skipNul = TRUE, warn = FALSE)) 
```

Create filters: stopwords, profanity, non-alphanumeric, url's, repeated letters

``` r
data("stop_words")
swear_words <- read_delim("./data/final/en_US/en_US.swearWords.csv", delim = "\n", col_names = FALSE)
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_character()
    ## )

``` r
swear_words <- unnest_tokens(swear_words, word, X1)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  
```

Clean dataframes for each souce.
Cleaning is separted from tidying so unnest\_tokens function can be used for words, and ngrams.

``` r
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
```

Create tidy dataframes for each source

``` r
tidy_blogs <- clean_blogs %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"
    ## Joining, by = "word"

``` r
tidy_news <- clean_news %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"
    ## Joining, by = "word"

``` r
tidy_twitter <- clean_twitter %>%  
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"
    ## Joining, by = "word"

Create tidy repository

``` r
tidy_repo <- bind_rows(mutate(tidy_blogs, source = "blogs"),
                       mutate(tidy_news,  source = "news"),
                       mutate(tidy_twitter, source = "twitter")) 
tidy_repo$source <- as.factor(tidy_repo$source)
```

Save tidy repository

``` r
saveRDS(tidy_repo, "./data/final/en_US/tidy_repo.rds")
(tidy_repo_size <- file.size("./data/final/en_US/tidy_repo.rds") / (2^20))
```

    ## [1] 82.07806

2. Most frequent words and word distributions
---------------------------------------------

``` r
freq <- tidy_repo %>%
<<<<<<< HEAD
  mutate(word = str_extract(word, "[a-z']+")) %>%
=======
>>>>>>> 2ea96a1e35c6f4524d85ce4ed6f08069668514e0
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))
```

Most frequent words

``` r
kable(head(freq, 10))
```

| word   |       n| source  |  proportion|
|:-------|-------:|:--------|-----------:|
| im     |  157940| twitter |   0.0158961|
| love   |  105474| twitter |   0.0106156|
| day    |   89821| twitter |   0.0090402|
| dont   |   88730| twitter |   0.0089304|
| rt     |   88189| twitter |   0.0088759|
| time   |   74547| twitter |   0.0075029|
| time   |   87526| blogs   |   0.0074466|
| lol    |   66386| twitter |   0.0066815|
| people |   51422| twitter |   0.0051754|
| people |   58839| blogs   |   0.0050059|

Least frequent words

``` r
freq_low <- freq %>% 
  arrange(proportion, n)
kable(head(freq_low, 10)) 
```

| word           |    n| source |  proportion|
|:---------------|----:|:-------|-----------:|
| aaaaaaaaaaaaaa |    1| blogs  |       1e-07|
| aabb           |    1| blogs  |       1e-07|
| aaberg         |    1| blogs  |       1e-07|
| aabergc        |    1| blogs  |       1e-07|
| aac            |    1| blogs  |       1e-07|
| aack           |    1| blogs  |       1e-07|
| aadvanced      |    1| blogs  |       1e-07|
| aafaton        |    1| blogs  |       1e-07|
| aafes          |    1| blogs  |       1e-07|
| aag            |    1| blogs  |       1e-07|

Word counts

``` r
# Number of unique words in repo
(repo_count <- tidy_repo %>%
  summarise(keys = n_distinct(word)))
```

    ## # A tibble: 1 x 1
    ##     keys
    ##    <int>
    ## 1 552745

``` r
# Potential to reduce repo size by cutoff proportion
(small_repo_count <- freq %>%
  filter(proportion > 0.0001) %>%
  summarise(keys = n_distinct(word)))
```

    ## # A tibble: 1 x 1
    ##    keys
    ##   <int>
    ## 1  2886

Number of words to attain 50% and 90% coverage of all words in repo

``` r
cover_50 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.5)
nrow(cover_50)
```

    ## [1] 1145

``` r
cover_90 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.5)
nrow(cover_90)
```

    ## [1] 1145

Word clouds

``` r
tidy_repo %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png)

Word clouds by source

``` r
# Blogs
tidy_repo %>%
  filter(source == "blogs") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-1.png)

``` r
# News
```

``` r
tidy_repo %>%
  filter(source == "news") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/chunkoptions-1.png)

``` r
# Twitter
tidy_repo %>%
  filter(source == "twitter") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/chunkoptions-2.png)

Word distribution

``` r
tidy_repo %>%
  count(word, sort = TRUE) %>%
  filter(n > 35000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

Word distribution by source

``` r
freq %>%
  filter(proportion > 0.002) %>% 
  mutate(word = reorder(word, proportion)) %>% 
  ggplot(aes(word, proportion)) +
  geom_col() + 
  xlab(NULL) + 
  coord_flip() +
  facet_grid(~source, scales = "free") 
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

``` r
################  
## ngrams
# 
# blogs_bigrams <- clean_blogs  %>%
#   unnest_tokens(bigram, text, token = "ngrams", n = 2)
# 
# news_bigrams <- clean_news  %>%
#   unnest_tokens(bigram, text, token = "ngrams", n = 2)
# 
# twitter_bigrams <- clean_twitter  %>%
#   unnest_tokens(bigram, text, token = "ngrams", n = 2)
# 
# #' Create tidy repository
# bigram_repo <- bind_rows(mutate(blogs_bigrams, source = "blogs"),
#                        mutate(news_bigrams,  source = "news"),
#                        mutate(twitter_bigrams, source = "twitter"))
# bigram_repo$source <- as.factor(bigram_repo$source)

end <- Sys.time()

(run_time <- end - start)
```

<<<<<<< HEAD
    ## Time difference of 6.851307 mins
=======
<<<<<<< HEAD
    ## Time difference of 5.425708 mins
=======
    ## Time difference of 6.948055 mins
>>>>>>> 2ea96a1e35c6f4524d85ce4ed6f08069668514e0
>>>>>>> 751e9f119504d12660f671bfa8879747fb38b092

``` r
################
```

------------------------------------------------------------------------

#### Session info:

``` r
sessionInfo()       
```

    ## R version 3.4.2 (2017-09-28)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 15063)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] bindrcpp_0.2       wordcloud_2.5      RColorBrewer_1.1-2
    ##  [4] knitr_1.17         stringr_1.2.0      dplyr_0.7.4       
    ##  [7] purrr_0.2.3        readr_1.1.1        tidyr_0.7.1       
    ## [10] tibble_1.3.4       ggplot2_2.2.1      tidyverse_1.1.1   
    ## [13] tidytext_0.1.4    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_0.2.2  slam_0.1-40       reshape2_1.4.2   
    ##  [4] haven_1.1.0       lattice_0.20-35   colorspace_1.3-2 
    ##  [7] htmltools_0.3.6   SnowballC_0.5.1   yaml_2.1.14      
    ## [10] rlang_0.1.2       foreign_0.8-69    glue_1.1.1       
    ## [13] modelr_0.1.1      readxl_1.0.0      bindr_0.1        
    ## [16] plyr_1.8.4        munsell_0.4.3     gtable_0.2.0     
    ## [19] cellranger_1.1.0  rvest_0.3.2       psych_1.7.8      
    ## [22] evaluate_0.10.1   labeling_0.3      forcats_0.2.0    
    ## [25] parallel_3.4.2    highr_0.6         broom_0.4.2      
    ## [28] tokenizers_0.1.4  Rcpp_0.12.13      backports_1.1.1  
    ## [31] scales_0.5.0      jsonlite_1.5      mnormt_1.5-5     
    ## [34] hms_0.3           digest_0.6.12     stringi_1.1.5    
    ## [37] grid_3.4.2        rprojroot_1.2     tools_3.4.2      
    ## [40] magrittr_1.5      lazyeval_0.2.0    janeaustenr_0.1.5
    ## [43] pkgconfig_2.0.1   Matrix_1.2-11     xml2_1.1.1       
    ## [46] lubridate_1.6.0   assertthat_0.2.0  rmarkdown_1.6    
    ## [49] httr_1.3.1        R6_2.2.2          nlme_3.1-131     
    ## [52] compiler_3.4.2
