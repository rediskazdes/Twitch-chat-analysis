library(stm)

load("~/shared/dota2-research/twitch/chat_data/ti7/data/meta.rda")

processed <- textProcessor(meta$documents, metadata = meta)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

fit <- stm(out$documents, 
           out$vocab, 
           prevalence =~ stage, 
           K = 100,
           data = out$meta, 
           init.type = "Spectral")
save(fit,out,processed,file="~/chats/data/new_model.rda")
