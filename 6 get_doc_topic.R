library(stm)
library(tidytext)
library(dplyr)
library(tidyr)

load(file="~/shared/dota2-research/twitch/chat_data/ti7/data/new_model.rda")
theta = tidy(fit,matrix="theta")
doc.topic = group_by(theta,document) %>% top_n(1)
texts = out$meta
texts$document = 1:nrow(texts)
texts = left_join(texts,doc.topic)

table(texts$event)
table(texts$topic)
table(texts$kill)
View(table(texts$kill,texts$topic))


save(theta,doc.topic,texts,file="~/chats/data/meta_with_topics.rda")
names(texts)
texts = select(texts,11,8,10)


View(filter(texts,topic==12))

