library(tidyverse)
library(stm)
library(tidytext)
library(ineq)
library(ggplot2)

load("~/chats/data/new_model.rda")
prep = estimateEffect(formula=1:100~stage, stmobj=fit,metadata=out$meta)

theta = tidy(fit,matrix="theta")
doc.topic = group_by(theta,document) %>% top_n(1)
texts = out$meta
texts$document = 1:nrow(texts)
texts = left_join(texts,doc.topic)
texts = inner_join(texts,meta)
View(table(texts$topic,texts$stage))

#### сравнение индекса джини за игру во все этапы
#### сравнение индекса джини за игру по группам и плей офф
#### сревнение индекса джини между этапами с финалом и без финала

load("~/chats/data/chat_with_meta.rda")

games_props = chat %>% group_by(id,viewer) %>% count()
games_props = games_props %>% group_by(id) %>% summarise(n_views = n_distinct(viewer),av_msgs = mean(n))
games_props = na.omit(games_props)

msgs_with_ids = texts %>% filter(!is.na(id)) %>% select(id,document)
msgs_with_ids = inner_join(msgs_with_ids,theta)


games_ginis = msgs_with_ids %>% group_by(document) %>% summarise(gini=ineq(gamma))
games_ginis = inner_join(games_ginis,msgs_with_ids %>% select(-topic,-gamma))
games_ginis = games_ginis[!duplicated(games_ginis),]
games_ginis = games_ginis %>% group_by(id) %>% summarise(gini=mean(gini))

ginis_view_stats = left_join(games_props,games_ginis)

ginis_view_stats$id = ginis_view_stats$id %>% as.character()
ginis_view_stats = left_join(ginis_view_stats,select(series.df,id,stage))
ginis_view_stats$stage[is.na(ginis_view_stats$stage)] = "quals"


ginis_view_stats %>% ggplot() + geom_point(aes(n_views,gini)) + geom_smooth(aes(n_views,gini)) + theme_bw()

cor.test(ginis_view_stats$gini,ginis_view_stats$n_views)
cor.test(ginis_view_stats$gini,ginis_view_stats$av_msgs)

ginis_view_stats$stage = factor(ginis_view_stats$stage,levels=c("quals","groups","playoff","finals"))

ginis_view_stats %>% ggplot() + geom_boxplot(aes(stage,gini)) + theme_bw()


# Kruskall
kruskal.test(gini ~ stage, data = ginis_view_stats)

# Pairwise Mann–Whitney U-test
pairwise.wilcox.test(ginis_view_stats$gini,ginis_view_stats$stage)






#### БЕЗ ТЕМЫ 24
theta_wo24 = filter(theta,topic!=24)

msgs_with_ids = texts %>% filter(!is.na(id)) %>% select(id,document)
msgs_with_ids = inner_join(msgs_with_ids,theta_wo24)

games_ginis1 = msgs_with_ids %>% group_by(document) %>% summarise(gini=ineq(gamma))
games_ginis1 = inner_join(games_ginis1,msgs_with_ids %>% select(-topic,-gamma))
games_ginis1 = games_ginis1[!duplicated(games_ginis1),]
games_ginis1 = games_ginis1 %>% group_by(id) %>% summarise(gini=mean(gini))


# альтернативный способ
games_ginis2 = msgs_with_ids %>% group_by(document) %>% summarise(gini=ineq(gamma))
games_ginis2 = games_ginis2 %>% inner_join(select(texts,document,vievers_per_7sec))

cor.test(games_ginis2$gini,log10(games_ginis2$vievers_per_7sec))

ggplot(data=games_ginis2) + geom_point(aes(x=log10(vievers_per_7sec),y=gini)) +
  geom_smooth(aes(x=log10(vievers_per_7sec),y=gini)) + theme_bw()


ginis_view_stats = left_join(games_props,games_ginis1)

ginis_view_stats$id = ginis_view_stats$id %>% as.character()
#ginis_view_stats = left_join(ginis_view_stats,select(series.df,id,stage))
#ginis_view_stats$stage[is.na(ginis_view_stats$stage)] = "quals"

ginis_view_stats %>% ggplot() + geom_point(aes(n_views,gini)) + geom_smooth(aes(n_views,gini)) + theme_bw()

cor.test(ginis_view_stats$gini,ginis_view_stats$n_views)
cor.test(ginis_view_stats$gini,ginis_view_stats$av_msgs)



ginis_view_stats$stage = factor(ginis_view_stats$stage,levels=c("quals","groups","playoff","finals"))

ginis_view_stats %>% ggplot() + geom_boxplot(aes(stage,gini)) + theme_bw()


# Kruskall
kruskal.test(gini ~ stage, data = ginis_view_stats)

# Pairwise Mann–Whitney U-test
pairwise.wilcox.test(ginis_view_stats$gini,ginis_view_stats$stage)


save(ginis_view_stats,file="data/stats_wo24.rda")



############# chat size and emoji usage

load("~/chats/data/chat_with_meta.rda")

chat_msgs = select(chat,viewer,id,text,stage)
chat_msgs = unnest_tokens(chat_msgs,word,text,token="regex",pattern=" ",to_lower=F)

game_word_freq = chat_msgs %>% group_by(id) %>% count() %>% na.omit()

load("/srv/store/students/bulygind/emotes_data.rda")
emotes_msgs = filter(chat_msgs,word %in% emotes_df$code) %>% group_by(id) %>% count()

msg_emot_freq = left_join(game_word_freq,emotes_msgs,by="id")
msg_emot_freq$sh = msg_emot_freq$n.y / msg_emot_freq$n.x
msg_emot_freq$id = as.character(msg_emot_freq$id)

ginis_view_stats = left_join(ginis_view_stats,msg_emot_freq)

df = ginis_view_stats
cor.test(df$n_views,df$sh)

df %>% ggplot() + geom_point(aes(n_views,sh)) + geom_smooth(aes(n_views,sh)) + theme_bw()


cor.test(df$n_views,df$n.x)
df %>% ggplot() + geom_point(aes(n_views,n.x)) + geom_smooth(aes(n_views,n.x)) + theme_bw()

cor.test(df$n_views,df$n.y)
df %>% ggplot() + geom_point(aes(n_views,n.y)) + geom_smooth(aes(n_views,n.y)) + theme_bw()


cor.test(df$n_views,df$av_msgs)
df %>% ggplot() + geom_point(aes(n_views,av_msgs)) + geom_smooth(aes(n_views,av_msgs)) + theme_bw()



##### ginis of users

view_distr = chat %>% select(id,viewer) %>% group_by(id,viewer) %>% count() %>% na.omit
viewers_gini = view_distr %>% group_by(id) %>% summarise(gini_vws = Gini(n))
viewers_gini$id = as.character(viewers_gini$id)
ginis_view_stats = left_join(viewers_gini,ginis_view_stats)


df = ginis_view_stats

cor.test(df$gini_vws,df$n_views)
df %>% ggplot() + geom_point(aes(n_views,gini_vws)) + geom_smooth(aes(n_views,gini_vws)) + theme_bw()


cor.test(df$gini_vws,df$gini,method="spearman")
df %>% ggplot() + geom_point(aes(gini,gini_vws)) + geom_smooth(aes(gini,gini_vws)) + theme_bw()


save(ginis_view_stats,file="/srv/store/students/bulygind/chats/data/stats_wo24.rda")



############# chat size and msg length

load("~/chats/data/chat_with_meta.rda")

chat_msgs = select(chat,id,text,viewer)
chat_msgs$msg_id = 1:nrow(chat_msgs)
chat_msgs = filter(chat_msgs,!is.na(chat_msgs$id))
chat_msgs = unnest_tokens(chat_msgs,word,text,to_lower=F)
