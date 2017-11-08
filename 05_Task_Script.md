Task 05: Creative Exploration
================
Mark Blackmore
2017-11-08

    ## Conflicts with tidy packages ----------------------------------------------

    ## Warning: package 'stringr' was built under R version 3.3.3

User Input

``` r
input <- data_frame(text = c("Heck of A Fine"))
```

Clean the Input

``` r
input_count <- str_count(input, boundary("word"))
input_words <- unlist(str_split(input, boundary("word")))
input_words <- tolower(input_words)
```

Call the matching functions

``` r
y <- ifelse(input_count == 1, bigram(input_words), 
  ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
```

Output

``` r
paste(input, y, sep = " ")
```

    ## [1] "Heck of A Fine line"

``` r
stop <- Sys.time()
(how_long <- stop - go)
```

    ## Time difference of 6.243035 secs
