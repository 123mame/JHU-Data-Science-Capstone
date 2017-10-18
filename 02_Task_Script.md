Task 2: Exploratory Data Analysis
================
Mark Blackmore
2017-10-18

English Repository Files

``` r
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  
```

Read the data files

``` r
blogs   <- readLines(blogs_file,   skipNul = TRUE)
news    <- readLines(news_file,    skipNul = TRUE)
```

    ## Warning in readLines(news_file, skipNul = TRUE): incomplete final line
    ## found on './data/final/en_US/en_US.news.txt'

``` r
twitter <- readLines(twitter_file, skipNul = TRUE) 
```

Create filters: profanity, non-alphanumeric characters, url's, repeated letters

``` r
swear_words <- read.csv("./data/final/en_US/en_US.swearWords.csv", header = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote =
    ## quote, : incomplete final line found by readTableHeader on './data/final/
    ## en_US/en_US.swearWords.csv'

``` r
swear_words <- gather(swear_words) %>% transmute(word = value)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

``` r
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"

tidy_blogs <- data_frame(text = blogs) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT")) %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"
    ## Joining, by = "word"

``` r
tidy_news <- data_frame(text = news) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT")) %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"
    ## Joining, by = "word"

``` r
tidy_twitter <- data_frame(text = twitter) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "ASCII//TRANSLIT")) %>%
  unnest_tokens(word, text) %>%
  anti_join(swear_words) %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"
    ## Joining, by = "word"

``` r
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
```

| word   |       n| source  |  proportion|
|:-------|-------:|:--------|-----------:|
| im     |  157940| twitter |   0.0158269|
| love   |  105474| twitter |   0.0105694|
| day    |   89821| twitter |   0.0090008|
| dont   |   88730| twitter |   0.0088915|
| rt     |   88189| twitter |   0.0088373|
| time   |   74547| twitter |   0.0074702|
| time   |   87526| blogs   |   0.0074219|
| lol    |   66386| twitter |   0.0066524|
| people |   51422| twitter |   0.0051529|
| people |   58839| blogs   |   0.0049893|

``` r
freq_low <- freq %>% 
  arrange(proportion, n)

kable(head(freq_low, 100)) 
```

| word                  |    n| source |  proportion|
|:----------------------|----:|:-------|-----------:|
| aaaaaaaaaaaaaa        |    1| blogs  |       1e-07|
| aabb                  |    1| blogs  |       1e-07|
| aaberg                |    1| blogs  |       1e-07|
| aabergc               |    1| blogs  |       1e-07|
| aac                   |    1| blogs  |       1e-07|
| aack                  |    1| blogs  |       1e-07|
| aadvanced             |    1| blogs  |       1e-07|
| aafaton               |    1| blogs  |       1e-07|
| aafes                 |    1| blogs  |       1e-07|
| aag                   |    1| blogs  |       1e-07|
| aahforget             |    1| blogs  |       1e-07|
| aahing                |    1| blogs  |       1e-07|
| aahs                  |    1| blogs  |       1e-07|
| aair                  |    1| blogs  |       1e-07|
| aaircare              |    1| blogs  |       1e-07|
| aajac                 |    1| blogs  |       1e-07|
| aakhri                |    1| blogs  |       1e-07|
| aakhu                 |    1| blogs  |       1e-07|
| aal                   |    1| blogs  |       1e-07|
| aalab                 |    1| blogs  |       1e-07|
| aalah                 |    1| blogs  |       1e-07|
| aalams                |    1| blogs  |       1e-07|
| aalaya                |    1| blogs  |       1e-07|
| aalesund              |    1| blogs  |       1e-07|
| aali                  |    1| blogs  |       1e-07|
| aalst                 |    1| blogs  |       1e-07|
| aamodt                |    1| blogs  |       1e-07|
| aamoth                |    1| blogs  |       1e-07|
| aan                   |    1| blogs  |       1e-07|
| aanchal               |    1| blogs  |       1e-07|
| aangc                 |    1| blogs  |       1e-07|
| aankh                 |    1| blogs  |       1e-07|
| aap                   |    1| blogs  |       1e-07|
| aapke                 |    1| blogs  |       1e-07|
| aarakshan             |    1| blogs  |       1e-07|
| aarc                  |    1| blogs  |       1e-07|
| aardema               |    1| blogs  |       1e-07|
| aare                  |    1| blogs  |       1e-07|
| aarhusians            |    1| blogs  |       1e-07|
| aarkstorecom          |    1| blogs  |       1e-07|
| aaronkaties           |    1| blogs  |       1e-07|
| aaronovitchs          |    1| blogs  |       1e-07|
| aaronson              |    1| blogs  |       1e-07|
| aarsman               |    1| blogs  |       1e-07|
| aarthi                |    1| blogs  |       1e-07|
| aartic                |    1| blogs  |       1e-07|
| aartis                |    1| blogs  |       1e-07|
| aaru                  |    1| blogs  |       1e-07|
| aase                  |    1| blogs  |       1e-07|
| aashikac              |    1| blogs  |       1e-07|
| aashimac              |    1| blogs  |       1e-07|
| aashish               |    1| blogs  |       1e-07|
| aasia                 |    1| blogs  |       1e-07|
| aastac                |    1| blogs  |       1e-07|
| aatask                |    1| blogs  |       1e-07|
| aati                  |    1| blogs  |       1e-07|
| aaton                 |    1| blogs  |       1e-07|
| aaunty                |    1| blogs  |       1e-07|
| aayan                 |    1| blogs  |       1e-07|
| aayeaye               |    1| blogs  |       1e-07|
| aayi                  |    1| blogs  |       1e-07|
| abaaabab              |    1| blogs  |       1e-07|
| ababased              |    1| blogs  |       1e-07|
| ababou                |    1| blogs  |       1e-07|
| abac                  |    1| blogs  |       1e-07|
| abaca                 |    1| blogs  |       1e-07|
| abackturned           |    1| blogs  |       1e-07|
| abacusc               |    1| blogs  |       1e-07|
| abadie                |    1| blogs  |       1e-07|
| abafazi               |    1| blogs  |       1e-07|
| abagnano              |    1| blogs  |       1e-07|
| abahlali              |    1| blogs  |       1e-07|
| abair                 |    1| blogs  |       1e-07|
| abajo                 |    1| blogs  |       1e-07|
| abakanowicz           |    1| blogs  |       1e-07|
| abakhan               |    1| blogs  |       1e-07|
| abalc                 |    1| blogs  |       1e-07|
| abalones              |    1| blogs  |       1e-07|
| abanc                 |    1| blogs  |       1e-07|
| abancay               |    1| blogs  |       1e-07|
| abandance             |    1| blogs  |       1e-07|
| abandonados           |    1| blogs  |       1e-07|
| abandonar             |    1| blogs  |       1e-07|
| abandonded            |    1| blogs  |       1e-07|
| abandonedeverything   |    1| blogs  |       1e-07|
| abandonedforthemoment |    1| blogs  |       1e-07|
| abandoningc           |    1| blogs  |       1e-07|
| abandonmentc          |    1| blogs  |       1e-07|
| abandonments          |    1| blogs  |       1e-07|
| abangida              |    1| blogs  |       1e-07|
| abaou                 |    1| blogs  |       1e-07|
| abaove                |    1| blogs  |       1e-07|
| abarouting            |    1| blogs  |       1e-07|
| abart                 |    1| blogs  |       1e-07|
| abarth                |    1| blogs  |       1e-07|
| abarthc               |    1| blogs  |       1e-07|
| abasa                 |    1| blogs  |       1e-07|
| abashedc              |    1| blogs  |       1e-07|
| abashidze             |    1| blogs  |       1e-07|
| abasiyanik            |    1| blogs  |       1e-07|

``` r
# Word cloud
tidy_blogs %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/chunk%20message-1.png)

``` r
tidy_blogs %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE)) 
```

![](02_Task_Script_files/figure-markdown_github-ascii_identifiers/chunk%20message-2.png)

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
    ## [22] evaluate_0.10.1   forcats_0.2.0     parallel_3.4.2   
    ## [25] highr_0.6         broom_0.4.2       tokenizers_0.1.4 
    ## [28] Rcpp_0.12.13      backports_1.1.1   scales_0.5.0     
    ## [31] jsonlite_1.5      mnormt_1.5-5      hms_0.3          
    ## [34] digest_0.6.12     stringi_1.1.5     grid_3.4.2       
    ## [37] rprojroot_1.2     tools_3.4.2       magrittr_1.5     
    ## [40] lazyeval_0.2.0    janeaustenr_0.1.5 pkgconfig_2.0.1  
    ## [43] Matrix_1.2-11     xml2_1.1.1        lubridate_1.6.0  
    ## [46] assertthat_0.2.0  rmarkdown_1.6     httr_1.3.1       
    ## [49] R6_2.2.2          nlme_3.1-131      compiler_3.4.2
