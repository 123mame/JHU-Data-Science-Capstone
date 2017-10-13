Exploring the `tm` Package
================
Mark Blackmore
2017-10-12

This document works through several examples in the article:
\* [Text mining infrastucture in R](http://www.jstatsoft.org/v25/i05/)

This is one of the references cited in the course document:
*Task 0: Understanding the Problem*

``` r
library(tm)
```

    ## Loading required package: NLP

Example with `ovid`
-------------------

### Find the sample corpus

``` r
txt <- system.file("texts", "txt", package = "tm")
```

### Load the corpus

``` r
(ovid <- Corpus(DirSource(txt, encoding = "UTF-8"),
                readerControl = list(reader = readPlain,
                                     language = "lat",
                                     load = TRUE)))
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 5

### Examine metadata

Show first document and update the author's name

``` r
meta(ovid[[1]])
```

    ##   author       : character(0)
    ##   datetimestamp: 2017-10-13 02:09:40
    ##   description  : character(0)
    ##   heading      : character(0)
    ##   id           : ovid_1.txt
    ##   language     : lat
    ##   origin       : character(0)

``` r
meta(ovid[[1]])$author <- "Publius Ovidius Naso"
meta(ovid[[1]])
```

    ##   author       : Publius Ovidius Naso
    ##   datetimestamp: 2017-10-13 02:09:40
    ##   description  : character(0)
    ##   heading      : character(0)
    ##   id           : ovid_1.txt
    ##   language     : lat
    ##   origin       : character(0)

``` r
# same result
ovid[[1]][2]
```

    ## $meta
    ##   author       : Publius Ovidius Naso
    ##   datetimestamp: 2017-10-13 02:09:40
    ##   description  : character(0)
    ##   heading      : character(0)
    ##   id           : ovid_1.txt
    ##   language     : lat
    ##   origin       : character(0)

### Examine the first document's text

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

### Concatenate several text collections to a single one

``` r
c(ovid[1:2], ovid[3:4])
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 4

### Show the number of documents in the corpus

``` r
length(ovid)
```

    ## [1] 5

### Show detailed summary of the text document collection

``` r
summary(ovid)
```

    ##            Length Class             Mode
    ## ovid_1.txt 2      PlainTextDocument list
    ## ovid_2.txt 2      PlainTextDocument list
    ## ovid_3.txt 2      PlainTextDocument list
    ## ovid_4.txt 2      PlainTextDocument list
    ## ovid_5.txt 2      PlainTextDocument list

### Show predefined transformations

Can be applied to a corupus with `tm_map`

``` r
getTransformations()
```

    ## [1] "removeNumbers"     "removePunctuation" "removeWords"      
    ## [4] "stemDocument"      "stripWhitespace"

#### Remove punctuation

``` r
ovid <- tm_map(ovid, FUN = removePunctuation)
ovid[[1]][1]
```

    ## $content
    ##  [1] "    Si quis in hoc artem populo non novit amandi"    
    ##  [2] "         hoc legat et lecto carmine doctus amet"     
    ##  [3] "    arte citae veloque rates remoque moventur"       
    ##  [4] "         arte leves currus arte regendus amor"       
    ##  [5] ""                                                    
    ##  [6] "    curribus Automedon lentisque erat aptus habenis" 
    ##  [7] "         Tiphys in Haemonia puppe magister erat"     
    ##  [8] "    me Venus artificem tenero praefecit Amori"       
    ##  [9] "         Tiphys et Automedon dicar Amoris ego"       
    ## [10] "    ille quidem ferus est et qui mihi saepe repugnet"
    ## [11] ""                                                    
    ## [12] "         sed puer est aetas mollis et apta regi"     
    ## [13] "    Phillyrides puerum cithara perfecit Achillem"    
    ## [14] "         atque animos placida contudit arte feros"   
    ## [15] "    qui totiens socios totiens exterruit hostes"     
    ## [16] "         creditur annosum pertimuisse senem"

#### Remove numbers

``` r
ovid <- tm_map(ovid, FUN = removeNumbers)
ovid[[2]][1]
```

    ## $content
    ##  [1] "    quas Hector sensurus erat poscente magistro" 
    ##  [2] "         verberibus iussas praebuit ille manus"  
    ##  [3] "    Aeacidae Chiron ego sum praeceptor Amoris"   
    ##  [4] "         saevus uterque puer natus uterque dea"  
    ##  [5] "    sed tamen et tauri cervix oneratur aratro"   
    ##  [6] ""                                                
    ##  [7] "         frenaque magnanimi dente teruntur equi" 
    ##  [8] "    et mihi cedet Amor quamvis mea vulneret arcu"
    ##  [9] "         pectora iactatas excutiatque faces"     
    ## [10] "    quo me fixit Amor quo me violentius ussit"   
    ## [11] "         hoc melior facti vulneris ultor ero"    
    ## [12] ""                                                
    ## [13] "    non ego Phoebe datas a te mihi mentiar artes"
    ## [14] "         nec nos aëriae voce monemur avis"       
    ## [15] "    nec mihi sunt visae Clio Cliusque sorores"   
    ## [16] "         servanti pecudes vallibus Ascra tuis"   
    ## [17] "    usus opus movet hoc vati parete perito"

#### Change to all lower case

``` r
ovid <- tm_map(ovid, FUN = content_transformer(tolower))
ovid[[5]][1]
```

    ## $content
    ##  [1] "    mater in aeneae constitit urbe sui"             
    ##  [2] "    seu caperis primis et adhuc crescentibus annis" 
    ##  [3] "         ante oculos veniet vera puella tuos"       
    ##  [4] "    sive cupis iuvenem iuvenes tibi mille placebunt"
    ##  [5] "         cogeris voti nescius esse tui"             
    ##  [6] ""                                                   
    ##  [7] "    seu te forte iuvat sera et sapientior aetas"    
    ##  [8] "         hoc quoque crede mihi plenius agmen erit"  
    ##  [9] "    tu modo pompeia lentus spatiare sub umbra"      
    ## [10] "         cum sol herculei terga leonis adit"        
    ## [11] "    aut ubi muneribus nati sua munera mater"        
    ## [12] ""                                                   
    ## [13] "         addidit externo marmore dives opus"        
    ## [14] "    nec tibi vitetur quae priscis sparsa tabellis"  
    ## [15] "         porticus auctoris livia nomen habet"       
    ## [16] "    quaque parare necem miseris patruelibus ausae"  
    ## [17] "         belides et stricto stat ferus ense pater"  
    ## [18] ""

#### Stem the corpus

``` r
# ovid <- tm_map(ovid, FUN = stemDocument)
# function is not available for language 'la'
```

#### Remove words

``` r
axe_words <- c("mater", "seu", "annis", "")
ovid <- tm_map(ovid, FUN = removeWords, axe_words)
ovid[[5]][1]
```

    ## $content
    ##  [1] "     in aeneae constitit urbe sui"                  
    ##  [2] "     caperis primis et adhuc crescentibus "         
    ##  [3] "         ante oculos veniet vera puella tuos"       
    ##  [4] "    sive cupis iuvenem iuvenes tibi mille placebunt"
    ##  [5] "         cogeris voti nescius esse tui"             
    ##  [6] ""                                                   
    ##  [7] "     te forte iuvat sera et sapientior aetas"       
    ##  [8] "         hoc quoque crede mihi plenius agmen erit"  
    ##  [9] "    tu modo pompeia lentus spatiare sub umbra"      
    ## [10] "         cum sol herculei terga leonis adit"        
    ## [11] "    aut ubi muneribus nati sua munera "             
    ## [12] ""                                                   
    ## [13] "         addidit externo marmore dives opus"        
    ## [14] "    nec tibi vitetur quae priscis sparsa tabellis"  
    ## [15] "         porticus auctoris livia nomen habet"       
    ## [16] "    quaque parare necem miseris patruelibus ausae"  
    ## [17] "         belides et stricto stat ferus ense pater"  
    ## [18] ""

#### Remove whitespace

``` r
ovid <- tm_map(ovid, FUN = stripWhitespace)
ovid[[3]][1]
```

    ## $content
    ##  [1] " vera canam coeptis amoris ades"                  
    ##  [2] " este procul vittae tenues insigne pudoris"       
    ##  [3] " quaeque tegis medios instita longa pedes"        
    ##  [4] " nos venerem tutam concessaque furta canemus"     
    ##  [5] " inque meo nullum carmine crimen erit"            
    ##  [6] ""                                                 
    ##  [7] " principio quod amare velis reperire labora"      
    ##  [8] " qui nova nunc primum miles in arma venis"        
    ##  [9] " proximus huic labor est placitam exorare puellam"
    ## [10] " tertius ut longo tempore duret amor"             
    ## [11] " hic modus haec nostro signabitur area curru"     
    ## [12] ""                                                 
    ## [13] " haec erit admissa meta terenda rota"             
    ## [14] " dum licet et loris passim potes ire solutis"     
    ## [15] " elige cui dicas tu mihi sola places"             
    ## [16] " haec tibi non tenues veniet delapsa per auras"   
    ## [17] " quaerenda est oculis apta puella tuis"

Example with `Reuters-21578`
----------------------------

``` r
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                   readerControl = list(reader = readReut21578XMLasPlain))
reuters[[1]][1]
```

    ## $content
    ## [1] "Diamond Shamrock Corp said that\neffective today it had cut its contract prices for crude oil by\n1.50 dlrs a barrel.\n    The reduction brings its posted price for West Texas\nIntermediate to 16.00 dlrs a barrel, the copany said.\n    \"The price reduction today was made in the light of falling\noil product prices and a weak crude oil market,\" a company\nspokeswoman said.\n    Diamond is the latest in a line of U.S. oil companies that\nhave cut its contract, or posted, prices over the last two days\nciting weak oil markets.\n Reuter"

### Eliminate Extra Whitespace

``` r
reuters <- tm_map(reuters, stripWhitespace)
```

### Convert to Lower Case

``` r
reuters <- tm_map(reuters, content_transformer(tolower))
```

### Stemming

``` r
tm_map(reuters, stemDocument)
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 20

### Remove Stop Words

``` r
reuters <- tm_map(reuters, removeWords, stopwords("english"))
reuters[[1]][1]
```

    ## $content
    ## [1] "diamond shamrock corp said  effective today   cut  contract prices  crude oil  1.50 dlrs  barrel.  reduction brings  posted price  west texas intermediate  16.00 dlrs  barrel,  copany said. \" price reduction today  made   light  falling oil product prices   weak crude oil market,\"  company spokeswoman said. diamond   latest   line  u.s. oil companies   cut  contract,  posted, prices   last two days citing weak oil markets. reuter"

### Creating Document-Term Matrices

``` r
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[5:10, 740:743])
```

    ## <<DocumentTermMatrix (documents: 6, terms: 4)>>
    ## Non-/sparse entries: 8/16
    ## Sparsity           : 67%
    ## Maximal term length: 6
    ## Weighting          : term frequency (tf)
    ## 
    ##      Terms
    ## Docs  one, opec opec's opec"s
    ##   211    0    0      0      0
    ##   236    0    6      0      2
    ##   237    0    1      0      0
    ##   242    1    2      0      0
    ##   246    0    1      1      0
    ##   248    0    6      0      0

### Operations on Document-Term Matrices

``` r
findFreqTerms(dtm, 5)
```

    ##  [1] "15.8"          "abdul-aziz"    "ability"       "accord"       
    ##  [5] "agency"        "agreement"     "ali"           "also"         
    ##  [9] "analysts"      "arab"          "arabia"        "barrel."      
    ## [13] "barrels"       "billion"       "bpd"           "budget"       
    ## [17] "company"       "crude"         "daily"         "demand"       
    ## [21] "dlrs"          "economic"      "emergency"     "energy"       
    ## [25] "exchange"      "expected"      "exports"       "futures"      
    ## [29] "government"    "group"         "gulf"          "help"         
    ## [33] "hold"          "industry"      "international" "january"      
    ## [37] "kuwait"        "last"          "market"        "may"          
    ## [41] "meeting"       "minister"      "mln"           "month"        
    ## [45] "nazer"         "new"           "now"           "nymex"        
    ## [49] "official"      "oil"           "one"           "opec"         
    ## [53] "output"        "pct"           "petroleum"     "plans"        
    ## [57] "posted"        "present"       "price"         "prices"       
    ## [61] "prices,"       "prices."       "production"    "quota"        
    ## [65] "quoted"        "recent"        "report"        "research"     
    ## [69] "reserve"       "reuter"        "said"          "said."        
    ## [73] "saudi"         "sell"          "sheikh"        "sources"      
    ## [77] "study"         "traders"       "u.s."          "united"       
    ## [81] "west"          "will"          "world"

### Find Associations Between Words

Find words associated with opec, with at least 0.8 correlation

``` r
findAssocs(dtm, "opec", 0.8)
```

    ## $opec
    ##   meeting emergency       oil      15.8  analysts    buyers      said 
    ##      0.88      0.87      0.87      0.85      0.85      0.83      0.82 
    ##   ability 
    ##      0.80

### Remove Sparse Terms

``` r
inspect(removeSparseTerms(dtm, 0.4))
```

    ## <<DocumentTermMatrix (documents: 20, terms: 3)>>
    ## Non-/sparse entries: 58/2
    ## Sparsity           : 3%
    ## Maximal term length: 6
    ## Weighting          : term frequency (tf)
    ## 
    ##      Terms
    ## Docs  oil reuter said
    ##   127   5      1    1
    ##   144  11      1    9
    ##   191   2      1    1
    ##   194   1      1    1
    ##   211   1      1    3
    ##   236   7      1    6
    ##   237   3      1    0
    ##   242   3      1    3
    ##   246   4      1    4
    ##   248   9      1    5
    ##   273   5      1    5
    ##   349   3      1    1
    ##   352   5      1    1
    ##   353   4      1    1
    ##   368   3      1    2
    ##   489   4      1    2
    ##   502   4      1    2
    ##   543   2      1    2
    ##   704   3      1    3
    ##   708   1      1    0

### Dictionary: Terms to Text Mine

``` r
inspect(DocumentTermMatrix(reuters,
                           list(dictionary = c("prices", "crude", "oil"))))
```

    ## <<DocumentTermMatrix (documents: 20, terms: 3)>>
    ## Non-/sparse entries: 41/19
    ## Sparsity           : 32%
    ## Maximal term length: 6
    ## Weighting          : term frequency (tf)
    ## 
    ##      Terms
    ## Docs  crude oil prices
    ##   127     2   5      3
    ##   144     0  11      3
    ##   191     2   2      0
    ##   194     3   1      0
    ##   211     0   1      0
    ##   236     1   7      2
    ##   237     0   3      0
    ##   242     0   3      1
    ##   246     0   4      0
    ##   248     0   9      7
    ##   273     5   5      4
    ##   349     2   3      0
    ##   352     0   5      4
    ##   353     2   4      1
    ##   368     0   3      0
    ##   489     0   4      2
    ##   502     0   4      2
    ##   543     2   2      2
    ##   704     0   3      2
    ##   708     1   1      0
