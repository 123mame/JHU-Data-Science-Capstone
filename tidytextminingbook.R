library(tidytext)
library(dplyr)
text_df <- data_frame(line = 1:length(twitter), text = twitter)
text_df
tidy_repo <- text_df %>% unnest_tokens(word, text)

data("stop_words")
tidy_repo <- tidy_repo %>%
   anti_join(stop_words)

tidy_repo %>%
  count(word, sort = TRUE)

text_df %>% unnest_tokens(ngram, text, token = "ngrams", n = 2)
