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
library(dplyr)
library(data.table)
library(ggthemes)

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
blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)
 
#' Read the data files
blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file, skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE) 

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

#' Max characters in a line, by file (longest line)
blogs_nchar_max   <- max(blogs_nchar)
news_nchar_max    <- max(news_nchar)
twitter_nchar_max <- max(twitter_nchar)

#' Median characters per file
blogs_nchar_med   <- median(blogs_nchar)
news_nchar_med    <- median(news_nchar)
twitter_nchar_med <- median(twitter_nchar)

#' Total characters per file
blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

#' Create summary of repo stats
repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           nchar_max  = c(blogs_nchar_max, news_nchar_max, twitter_nchar_max),
                           nchar_med =  c(blogs_nchar_med, news_nchar_med, twitter_nchar_med),
                           nchar_sum =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum))


repo_summary <- repo_summary %>% mutate(pct_nchar = round(nchar_sum/sum(nchar_sum), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
kable(repo_summary)

#' ## 2. Sample the data and save the sample
#' 
#' Compute sample sizes in terms of lines
sample_pct = 0.05
set.seed(1001)
blogs_size <- blogs_lines * sample_pct
news_size  <- news_lines * sample_pct
twitter_size <- twitter_lines * sample_pct
 
#' Create samples
blogs_sample <- sample(blogs, blogs_size)
news_sample  <- sample(news, news_size)
twitter_sample <- sample(twitter, twitter_size)
repo_sample = c(blogs_sample, news_sample, twitter_sample)

#' Save sample
writeLines(repo_sample, "./data/final/en_US/en_US.repo_sample.txt")
saveRDS(repo_sample, file = "./data/final/en_US/repo_sample.RData" )

########################## START HERE ############### SEE LAST TAB
#' [Test Mining Tutorial]("https://www.hackerearth.com/fr/practice/machine-learning/advanced-techniques/text-mining-feature-engineering-r/tutorial/")
#'

#' ## 3.  Clean the sample data
#' Use `tm` to create and clean the corpus
clean_sample <- Corpus(VectorSource(repo_sample),
                       readerControl = list(readPlain, 
                                            language = "en",
                                            load = TRUE))
print(as.character(clean_sample[[1]]))

#' Transform sample to all lower case
clean_sample <- tm_map(clean_sample, content_transformer(tolower))

#' Remove URL's  
#' Source: [R and Data Mining]("http://www.rdatamining.com/books/rdm/faq/removeurlsfromtext")
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
clean_sample <- tm_map(clean_sample, content_transformer(removeURL))

# Remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
clean_sample <- tm_map(clean_sample, content_transformer(removeNumPunct))

#' Create profanity filter  
#' Source: [List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words]("List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/en")
profanity <- read.table("./data/final/en_US/profanity.txt", header = FALSE, sep ="\n")

#' Remove profanity
clean_sample <- tm_map(clean_sample, removeWords, profanity[,1])

#' Remove stopwords
clean_sample <- tm_map(clean_sample, removeWords, stopwords("english"))
clean_sample <- tm_map(clean_sample, removeWords, stopwords("SMART"))
print(as.character(clean_sample[[1]]))

#' Remove Whitespace
clean_sample <- tm_map(clean_sample, stripWhitespace)
print(as.character(clean_sample[[1]]))

#' Save clean corpus
saveRDS(clean_sample, file = "./data/final/en_US/clean_sample.RData" )

#' Convert to text document
text_corpus <- tm_map(clean_sample, PlainTextDocument)

#perform stemming - this should always be performed after text doc conversion
text_corpus <- tm_map(text_corpus, stemDocument,language = "english")
print(as.character(text_corpus[[1]]))
text_corpus[[1]]$content

#convert to document term matrix
docterm_corpus <- DocumentTermMatrix(text_corpus)
dim(docterm_corpus)

new_docterm_corpus <- removeSparseTerms(docterm_corpus,sparse = 0.99)
dim(new_docterm_corpus)

#find frequent terms
colS <- colSums(as.matrix(new_docterm_corpus))
length(colS)
doc_features <- data.table(name = attributes(colS)$names, count = colS)

#most frequent and least frequent words
doc_features[order(-count)][1:10] #top 10 most frequent words
doc_features[order(count)][1:10] #least 10 frequent words


ggplot(doc_features[count>5000],aes(name, count)) +
  geom_bar(stat = "identity",fill='lightblue',color='black') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_economist() + scale_color_economist() 

# #check association of terms of top features
# findAssocs(new_docterm_corpus, "time", corlimit = 0.5)
# findAssocs(new_docterm_corpus, "love", corlimit = 0.5)
# findAssocs(new_docterm_corpus, "day",  corlimit = 0.5)

library(wordcloud)
wordcloud(names(colS), colS, min.freq = 500, 
          colors = brewer.pal(6, 'Dark2'), random.order = FALSE)  

wordcloud(names(colS), colS, min.freq = 2000, 
          colors = brewer.pal(6, 'Dark2'), random.order = FALSE)  


