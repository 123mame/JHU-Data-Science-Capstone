Task 1: Getting and Cleaning the Data
================
Mark Blackmore
October 7, 2017

Reading pieces of the file at a time will require the use of a file connection in R. The following code could be used to read the first few lines of the English Twitter dataset:

Read the first line of text

``` r
con <- file("./data/final/en_US/en_US.twitter.txt", "r") 
readLines(con, 1) 
```

    ## [1] "How are you? Btw thanks for the RT. You gonna be in DC anytime soon? Love to see you. Been way, way too long."

Read the next line of text

``` r
readLines(con, 1) 
```

    ## [1] "When you meet someone special... you'll know. Your heart will beat more rapidly and you'll smile for no reason."

Read in the next 5 lines of text

``` r
readLines(con, 5) 
```

    ## [1] "they've decided its more fun if I don't."                                                             
    ## [2] "So Tired D; Played Lazer Tag & Ran A LOT D; Ughh Going To Sleep Like In 5 Minutes ;)"                 
    ## [3] "Words from a complete stranger! Made my birthday even better :)"                                      
    ## [4] "First Cubs game ever! Wrigley field is gorgeous. This is perfect. Go Cubs Go!"                        
    ## [5] "i no! i get another day off from skool due to the wonderful snow (: and THIS wakes me up...damn thing"

It's important to close the connection when you are done

``` r
close(con) 
```
