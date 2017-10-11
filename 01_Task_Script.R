#' ---
#' title: "Task 1: Getting and Cleaning the Data"
#' author: "Mark Blackmore"
#' date: "October 7, 2017"
#' output: github_document
#' ---
#'   
#' Reading pieces of the file at a time will require the use of a file connection
#' in R.  The following code could be used to read the first few lines of the English 
#' Twitter dataset:  
#' 
#' Read the first line of text  
con <- file("./data/final/en_US/en_US.twitter.txt", "r") 
readLines(con, 1) 

#' Read the next line of text 
readLines(con, 1) 

#' Read in the next 5 lines of text 
readLines(con, 50) 

#' It's important to close the connection when you are done
close(con) 

?readLines


