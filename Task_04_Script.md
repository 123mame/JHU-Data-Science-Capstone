Task 04: Prediction Model
================
Mark Blackmore
October 26, 2017

``` r
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
  library(ngram)
})
```

``` r
tidy_repo <- readRDS("./clean_repos/tidy_repo.rds")
cover_90  <- readRDS("./clean_repos/cover_90.rds")
bigram_cover_90   <- readRDS("./clean_repos/bigram_cover_90.rds")
trigram_cover_90  <- readRDS("./clean_repos/trigram_cover_90.rds")
quadgram_cover_90 <- readRDS("./clean_repos/quadgram_cover_90.rds")
```
