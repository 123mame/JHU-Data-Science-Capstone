#' ---
#' title: 'Task 04: Prediction Model'
#' author: "Mark Blackmore"
#' date: "October 26, 2017"
#' output: github_document
#' ---

#+ setup
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
  library(ngram)
})

#' Load a ten percent sample of the corpus
