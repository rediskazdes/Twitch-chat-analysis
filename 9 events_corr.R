library(dplyr)
library(tidyr)
library(tseries)

load(file="~/chats/data/meta_with_topics.rda")
texts1 = filter(texts,!is.na(id))
texts1 = arrange(texts1,time_7sec)
#texts1 = texts

topic_curve_calculate <- function(x) {
  topic_of_interest1 <- texts1$topic == x
  corr <- ccf(topic_of_interest1, texts1$kill + texts1$event, lag.max=20, plot=F,type="correlation")
  return(as.vector(corr$acf))
}


all_topics = unique(texts1$topic)
topic_curves <- sapply(all_topics, topic_curve_calculate)



rownames(topic_curves) <- 1:41
colnames(topic_curves) <- 1:110
res.cor <- cor(topic_curves, method = "spearman")
rownames(res.cor) <- 1:100
colnames(res.cor) <- 1:100
topics_dist <- as.dist((1 - res.cor)/2)


library(factoextra)
fviz_nbclust(as.matrix(topics_dist), hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

topics_clusters = hclust(topics_dist)
plot(topics_clusters)
cluster_membership <- stats::cutree(topics_clusters, k=4)
cluster_membership


topic_curves = as.data.frame(t(topic_curves))
topic_curves$cluster = cluster_membership
topic_curves$topic = 1:length(all_topics)
topic_dist_time = topic_curves %>% gather(time_slot,prob,1:41)
time_slot = topic_dist_time$time_slot %>% unique()
time_slot = as.data.frame(time_slot)
time_slot$number = extract_numeric(time_slot$time_slot)
time_slot$number = time_slot$number - median(time_slot$number)
topic_dist_time = left_join(topic_dist_time,time_slot)
topic_dist_time$number = as.factor(topic_dist_time$number)

df = filter(topic_dist_time,cluster %in% unique(cluster_membership))

#df = filter(topic_dist_time,cluster %in% 5)
#unique(df$topic)


df$number = as.numeric(as.character(df$number))
df$number = as.factor(df$number)

ggplot(data=df) + geom_boxplot(aes(x=number,y=prob),colour="#000099") +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 21,colour="#CC0000") +
  theme_bw() + labs(x="Time Lag",y = "Correlation") +
  facet_wrap(~cluster,scale="free_y") + theme_bw() + theme(text = element_text(size=25))

topic_clust = topic_curves %>% select(topic,cluster)
save(topic_clust,file="data/topic_clusters.rda")

# TOPICS IN CLUSTERS

load(file="~/chats/data/topic_words_mtrx.rda")

top3words = topic_words_matrx %>%
  select(topic, beta, term) %>% group_by(topic) %>% 
  top_n(3,beta) %>% arrange(-beta) %>% 
  summarise(terms = paste0(term,collapse=" "))

top3words$cluster = cluster_membership
