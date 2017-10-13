Task 1: Getting and Cleaning the Data
================
Mark Blackmore
2017-10-12

``` r
library(downloader)
library(tm)
```

    ## Loading required package: NLP

Create a data directory

``` r
if (!file.exists("data")) {
  dir.create("data")
}
```

Download the data

``` r
# url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# download(url, dest="dataset.zip", mode="wb") 
# unzip ("dataset.zip", exdir = "./data")
```

English Repository Files

``` r
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.news.txt"
```

Reading pieces of the file at a time will require the use of a file connection in R. The following code could be used to read the first few lines of the English Twitter dataset:

Read the first line of text

``` r
con <- file(twitter_file, "r") 
twitter <-  readLines(con, 1) 
```

Read the next line of text

``` r
readLines(con, 1) 
```

    ## [1] "The St. Louis plant had to close. It would die of old age. Workers had been making cars there since the onset of mass automotive production in the 1920s."

Read in the next 5 lines of text

``` r
readLines(con, 5) 
```

    ## [1] "WSU's plans quickly became a hot topic on local online sites. Though most people applauded plans for the new biomedical center, many deplored the potential loss of the building."                                                                                                                                                                                                                                                                                                                                 
    ## [2] "The Alaimo Group of Mount Holly was up for a contract last fall to evaluate and suggest improvements to Trenton Water Works. But campaign finance records released this week show the two employees donated a total of $4,500 to the political action committee (PAC) Partners for Progress in early June. Partners for Progress reported it gave more than $10,000 in both direct and in-kind contributions to Mayor Tony Mack in the two weeks leading up to his victory in the mayoral runoff election June 15."
    ## [3] "And when it's often difficult to predict a law's impact, legislators should think twice before carrying any bill. Is it absolutely necessary? Is it an issue serious enough to merit their attention? Will it definitely not make the situation worse?"                                                                                                                                                                                                                                                            
    ## [4] "There was a certain amount of scoffing going around a few years ago when the NFL decided to move the draft from the weekend to prime time -- eventually splitting off the first round to a separate day."                                                                                                                                                                                                                                                                                                          
    ## [5] "14915 Charlevoix, Detroit"

It's important to close the connection when you are done

``` r
close(con) 
```
