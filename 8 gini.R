library(dplyr)
library(ineq)
library(tidyr)

# Gini with liquid topic (94)
load(file="~/chats/data/meta_with_topics.rda")
matches = filter(texts,!is.na(id))
topic_distr = matches %>% group_by(id,topic,stage) %>% count()
ginis = topic_distr %>% group_by(stage) %>% summarise(gini=Gini(n))

# Gini without liqid topic (94)

load("~/chats/data/new_model.rda")

theta = filter(theta,topic!=94)
doc.topic = group_by(theta,document) %>% top_n(1)
texts = out$meta
texts$document = 1:nrow(texts)
texts = left_join(texts,doc.topic)

matches = filter(texts,!is.na(id))
topic_distr = matches %>% group_by(id,topic,stage) %>% count()
ginis_wo94 = topic_distr %>% group_by(stage) %>% summarise(gini=Gini(n))



load(file="~/chats/data/chat_with_meta.rda")

meta2 = filter(texts,stage=="groups")
gini_per_games = meta2  %>% group_by(stage,id,topic) %>% count() %>% group_by(stage,id) %>% summarise(gini=Gini(n)) 
viewer_per_games = chat %>% group_by(id,viewer) %>% count() %>% group_by(id) %>% count()
gini_per_games = inner_join(gini_per_games,viewer_per_games)
gini_per_games = na.omit(gini_per_games)

summary(lm(gini~nn+stage,data=gini_per_games))

ggplot() + geom_point(data=gini_per_games,aes(x=log10(nn),y=gini))


