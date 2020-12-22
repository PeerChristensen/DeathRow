# death row wordcloud

library(spacyr)
library(wordcloud)
library(tidyverse)

spacy_initialize()

df <- read_csv("last_statements.csv")

tagged <- spacy_parse(df$Statement)

nouns <- tagged %>%
  as_tibble() %>%
  filter(pos=="NOUN") %>%
  filter(lemma != "statement") %>%
  count(lemma)

wordcloud(nouns$lemma,nouns$n,colors=c("black","orange","orange","orange","darkred"))
