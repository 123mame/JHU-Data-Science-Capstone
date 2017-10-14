#' ---
#' title: "Task 1: Getting and Cleaning the Data"
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
library(downloader)
library(tm)
library(knitr)
library(tidyverse)

#' ## 1. Download and explore the data
#'
#' Create a data directory  
if (!file.exists("data")) {
  dir.create("data")
}

#' Download the data
# url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# download(url, dest="dataset.zip", mode="wb") 
# unzip ("dataset.zip", exdir = "./data")

#' English Repository Files
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  

#' File Sizes (Mb)
blogs_size <- file.size(blogs_file) / (2^20)
news_size  <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)

#' Read the data files
blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file, skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE) 

#' Number of Lines per file
blogs_lines <- length(blogs)
news_lines  <- length(news)
twitter_lines <- length(twitter)
total_lines  <- blogs_lines + news_lines + twitter_lines

#' Distibution of characters per line, by file
blogs_nchar <- nchar(blogs)
news_nchar  <- nchar(news)
twitter_nchar <- nchar(twitter)
boxplot(blogs_nchar, news_nchar, twitter_nchar, log = "y",
        names = c("blogs", "news", "twitter"),
        ylab = "log(Number of Characters)", xlab = "File Name")
title("Comparing Distributions of Chracters per Line")

#' Max characters in a line, by file (longest line)
blogs_nchar_max <- max(blogs_nchar)
news_nchar_max  <- max(news_nchar)
twitter_nchar_max <- max(twitter_nchar)

#' Median characters per file
blogs_nchar_med <- median(blogs_nchar)
news_nchar_med  <- median(news_nchar)
twitter_nchar_med <- median(twitter_nchar)

#' Total characters per file
blogs_nchar_sum <- sum(blogs_nchar)
news_nchar_sum  <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)


repo_summary <- data.frame(file_names = c("blogs", "news", "twitter"),
                           file_size  = c(blogs_size, news_size, twitter_size),
                           file_lines = c(blogs_lines, news_lines, twitter_lines),
                           #nchar_max  = c(blogs_nchar_max, news_nchar_max, twitter_nchar_max),
                           #nchar_med =  c(blogs_nchar_med, news_nchar_med, twitter_nchar_med),
                           nchar_sum =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum))


repo_summary <- repo_summary %>% mutate(pct_nchar = round(nchar_sum/sum(nchar_sum), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(file_lines/sum(file_lines), 2))
kable(repo_summary)

#' ## 2. Sample and clean the data
#' 
#' Compute sample sizes in terms of lines
blogs_size <- repo_summary$pct_nchar[1] * total_lines
news_size  <- repo_summary$pct_nchar[2] * total_lines
twitter_size <- repo_summary$pct_nchar[3] * total_lines

#' Create samples
blogs_sample <- sample(blogs, blogs_size)
news_sample  <- sample(news, news_size)
twiter_sample <- sample(twitter, twitter_size)


