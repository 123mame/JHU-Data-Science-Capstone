#' ---
#' title: "Task 1: Getting and Cleaning the Data"
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
library(downloader)
library(tm)
#'
#' Create a data directory  
if (!file.exists("data")) {
  dir.create("data")
}

#' Download the data
#+ eval = FALSE
# url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# download(url, dest="dataset.zip", mode="wb") 
# unzip ("dataset.zip", exdir = "./data")

#' English Repository Files
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"


#' Reading pieces of the file at a time will require the use of a file connection
#' in R.  The following code could be used to read the first few lines of the English 
#' Twitter dataset:  
#' 
#' Read the first line of text  
twitter <-  readLines(twitter_file, skipNul = TRUE) 







