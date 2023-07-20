library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)


load(file="~/chats/data/chat_with_meta.rda")

chat$time_7sec = ceiling_date(chat$time_msc,"7 sec")

meta = chat %>% group_by(time_7sec,stage,id,is_picks,is_teamfight) %>%
  summarise(documents=paste0(text,collapse=" "), vievers_per_7sec = n_distinct(viewer))

events = chat %>% select(time_msc, time_7sec,stage,kill,event)
events = events[!duplicated(events),]
events$kill[is.na(events$kill)] = 0
events$event[is.na(events$event)] = 0

meta = events %>% group_by(time_7sec) %>% summarise(event=sum(event),kill=sum(kill)) %>% right_join(meta,by="time_7sec")
meta$stage = factor(meta$stage,levels=c("qualifiers","groups","playoff","finals"))
meta$documents = str_replace_all(meta$documents,fixed("322"),"thtwtw")

save(meta,file="~/chats/data/meta.rda")
