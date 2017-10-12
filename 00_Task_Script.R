#' ---
#' title: "Task 0: understanding the Problem"
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' ## Exploring the `tm` package
#'
library(tm)

#' Find the sample corpus
txt <- system.file("texts", "txt", package = "tm")

#' Load the corpus
(ovid <- Corpus(DirSource(txt),
                readerControl = list(reader = readPlain,
                                     language = "la",
                                     load = TRUE)))

#' Examine metadata from first document and update the author's name
meta(ovid[[1]])
meta(ovid[[1]])$author <- "Publius Ovidius Naso"
meta(ovid[[1]])
# same result
ovid[[1]][2]

#' Examine the first document's text
ovid[[1]][1]

#' Concatenates several text collections to a single one
c(ovid[1:2], ovid[3:4])
