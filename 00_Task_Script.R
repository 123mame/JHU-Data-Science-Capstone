---
title: "Syllabus"
author: "Mark Blackmore"
date: "October 1, 2017"
output: github_document
---

library(downloader)
library(tm)
  
if (!file.exists("data")) {
    dir.create("data")
}


# url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# download(url, dest="dataset.zip", mode="wb") 
# unzip ("dataset.zip", exdir = "./data")

de_files <- "./data/final/de_DE/"
us_files <- "./data/final/en_US/"
fi_files <- "./data/final/fi_FI/"
ru_files <- "./data/final/ru_RU/"

us_txts <- VCorpus(DirSource(us_files), 
                 readerControl = list(language = "en"))
us_blogs <- us_txts[[1]]
us_blogs[[1]] # corpus
us_blogs[[2]]  # metadata
# lapply(us_blogs[1], as.character)
