Task 0: understanding the Problem
================
Mark Blackmore
2017-10-12

Exploring the `tm` package
--------------------------

``` r
library(tm)
```

    ## Loading required package: NLP

Find the sample corpus

``` r
txt <- system.file("texts", "txt", package = "tm")
```

Load the corpus

``` r
(ovid <- Corpus(DirSource(txt),
                readerControl = list(reader = readPlain,
                                     language = "la",
                                     load = TRUE)))
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 5

Examine metadata from first document and update the author's name

``` r
meta(ovid[[1]])
```

    ##   author       : character(0)
    ##   datetimestamp: 2017-10-12 22:57:35
    ##   description  : character(0)
    ##   heading      : character(0)
    ##   id           : ovid_1.txt
    ##   language     : la
    ##   origin       : character(0)

``` r
meta(ovid[[1]])$author <- "Publius Ovidius Naso"
meta(ovid[[1]])
```

    ##   author       : Publius Ovidius Naso
    ##   datetimestamp: 2017-10-12 22:57:35
    ##   description  : character(0)
    ##   heading      : character(0)
    ##   id           : ovid_1.txt
    ##   language     : la
    ##   origin       : character(0)

``` r
# same result
ovid[[1]][2]
```

    ## $meta
    ##   author       : Publius Ovidius Naso
    ##   datetimestamp: 2017-10-12 22:57:35
    ##   description  : character(0)
    ##   heading      : character(0)
    ##   id           : ovid_1.txt
    ##   language     : la
    ##   origin       : character(0)

Examine the first document's text

``` r
ovid[[1]][1]
```

    ## $content
    ##  [1] "    Si quis in hoc artem populo non novit amandi,"    
    ##  [2] "         hoc legat et lecto carmine doctus amet."     
    ##  [3] "    arte citae veloque rates remoque moventur,"       
    ##  [4] "         arte leves currus: arte regendus amor."      
    ##  [5] ""                                                     
    ##  [6] "    curribus Automedon lentisque erat aptus habenis," 
    ##  [7] "         Tiphys in Haemonia puppe magister erat:"     
    ##  [8] "    me Venus artificem tenero praefecit Amori;"       
    ##  [9] "         Tiphys et Automedon dicar Amoris ego."       
    ## [10] "    ille quidem ferus est et qui mihi saepe repugnet:"
    ## [11] ""                                                     
    ## [12] "         sed puer est, aetas mollis et apta regi."    
    ## [13] "    Phillyrides puerum cithara perfecit Achillem,"    
    ## [14] "         atque animos placida contudit arte feros."   
    ## [15] "    qui totiens socios, totiens exterruit hostes,"    
    ## [16] "         creditur annosum pertimuisse senem."

Concatenates several text collections to a single one

``` r
c(ovid[1:2], ovid[3:4])
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 4
