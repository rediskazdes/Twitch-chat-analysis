library(stringr)
library(ggrepel)
library(googlesheets)
library(stm)
library(purrr)
library(dplyr)
library(tidyr)

load("~/chats/data/new_model.rda")
prep = estimateEffect(formula=1:100~stage, stmobj=fit,metadata=out$meta)

heh = summary(prep)
reg_summ = heh$tables %>% map(broom::tidy) %>% map_df(magrittr::extract,names(.[[1]]))

topic_labels = read.csv("~/chats/data/topic_labels.csv")

reg_summ$label = rep(topic_labels$label,each=4)
reg_summ$object = rep(topic_labels$object,each=4)
reg_summ$topic = rep(topic_labels$topic,each=4)
reg_summ$pract = rep(topic_labels$practice,each=4)
reg_summ$form = rep(topic_labels$form,each=4)

names(reg_summ) = c("var","coef","se","tval","pval","label","object","topic","pract","form")
reg_summ$var = str_replace(reg_summ$var,fixed("(Intercept)"),"stagequals")

topic_prev = select(reg_summ,var,coef,topic,object,label,pract,form)
topic_prev = topic_prev %>% spread(var,coef)
topic_prev$stagegroups = topic_prev$stagequals + topic_prev$stagegroups
topic_prev$stageplayoff = topic_prev$stagequals + topic_prev$stageplayoff
topic_prev$stagefinals = topic_prev$stagequals + topic_prev$stagefinals
topic_prev = topic_prev %>% gather(var,coef,6:9)
topic_prev = left_join(topic_prev,select(reg_summ,topic,var,pval))

topic_prev = dplyr::filter(topic_prev,pval < 0.05)
names(topic_prev)[6] = "stage"

stages_proports = topic_prev
topic_prev[is.na(topic_prev)] = -99
#topic_prev = topic_prev %>% gather("stage","coef", 5:7)
#stages_proports %>% ggplot + geom_boxplot(aes(x=,y=coef,color=var))

topic_prev_max = topic_prev %>% group_by(topic) %>% summarise(max=max(coef))
topic_prev = left_join(topic_prev,topic_prev_max)
topic_prev$prev_stage = ifelse(topic_prev$coef == topic_prev$max,1,0)
topic_prev$stage = str_replace(topic_prev$stage,"stage","")
topic_prev = filter(topic_prev,coef > -99)

topic_prev$stage = factor(topic_prev$stage,levels=c("quals","groups","playoff","finals"))
topic_prev = select(topic_prev,1,3,2,4,5,6,7,8,9,10)
topic_prev=unite(topic_prev,new_label,2:5,sep=" | ",remove=F)
topic_prev=unite(topic_prev,topic_cat,4:6,sep=" | ",remove=F)
topic_prev=unite(topic_prev,label_form,c(3,7),sep=" | ",remove=F)



topics_aggr = filter(topic_prev,prev_stage == 1)
topics_aggr = topics_aggr %>% group_by(stage,label_form) %>% summarise(preval = sum(coef))



topics_aggr %>% 
  ggplot +
  aes(x=1,y=1,label = label_form,size=preval) +
  scale_size(range = c(5, 12)) +
  geom_text_repel(segment.size = 0,force = 30) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  labs(x = '', y = '') +
  theme_bw() + 
  facet_wrap(~stage,scales="free") +
  scale_fill_brewer(palette="Dark2",aesthetics = "colour") +
  theme(text = element_text(size=4))

