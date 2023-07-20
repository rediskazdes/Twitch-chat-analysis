library(dplyr)
library(tidyr)
library(stringr)

load(file="~/chats/data/meta_with_topics.rda")


topic_labels = read.csv("~/chats/data/topic_labels.csv")
labels_df = topic_labels %>% group_by(practice,form,object) %>% count()

label_docs = doc.topic %>% left_join(select(topic_labels,label,topic,practice,object,form))
label_docs = label_docs %>% group_by(practice,form,object) %>% count()
label_docs = topic_labels %>% group_by(practice,form,object) %>%
             summarise(labels=paste(label,collapse=" | ")) %>% right_join(label_docs) 

label_docs$share = label_docs$n / sum(label_docs$n)
label_docs$share = label_docs$share %>% round(3)
label_docs$share = label_docs$share * 100
label_docs = arrange(label_docs,-n)


label_docs$share = label_docs$share %>% str_c(.," %")
label_docs = arrange(label_docs,-n)
label_docs = select(label_docs,-n)

num_topics = topic_labels %>% group_by(practice,form,object) %>% count()
label_docs = left_join(label_docs,num_topics)


write.csv(label_docs,file="data/label_docs.csv")


#### topic clusters

load(file="data/topic_clusters.rda")

label_clust = left_join(topic_labels,topic_clust)
heh = label_clust %>% group_by(cluster,practice) %>% count()
