library(tidytext)
library(tidyverse)
library(googlesheets)

load(file="~/chats/data/new_model.rda")

topic_words = tidy(fit,matrix="beta") %>%
  select(topic, beta, term) %>% group_by(topic) %>% 
  top_n(10,beta) %>% arrange(-beta) %>% 
  summarise(terms = paste0(term,collapse=" "))


# boring_ss <- gs_new("TI7 STM 0711", ws_title = "topic words", input = topic_words,
#                     trim = TRUE, verbose = FALSE)
topic_words_matrx = tidy(fit,matrix="beta")




save(topic_words_matrx,topic_words,file="~/chats/data/topic_words_mtrx.rda")

