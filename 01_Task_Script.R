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
twitter_file <- "./data/final/en_US/en_US.news.txt"


#' Reading pieces of the file at a time will require the use of a file connection
#' in R.  The following code could be used to read the first few lines of the English 
#' Twitter dataset:  
#' 
#' Read the first line of text  
con <- file(twitter_file, "r") 
twitter <-  readLines(con, 1) 

#' Read the next line of text 
readLines(con, 1) 

#' Read in the next 5 lines of text 
readLines(con, 5) 

#' It's important to close the connection when you are done
close(con) 




