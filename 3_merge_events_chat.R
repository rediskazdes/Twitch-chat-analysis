library(lubridate)

# предобработка датафреймов

load("~/chats/data/chat.rda")
load("~/chats/data/events_ti7.rda")


# matching games and chats

chat$id = NA
chat$is_picks = NA
chat$is_teamfight = NA
chat$id_w_breaks = NA

i= m.dfs$id[1]
  for (i in m.dfs$id){
    int = interval(m.dfs[m.dfs$id==i,]$start,m.dfs[m.dfs$id==i,]$end)
    chat$id[chat$time_msc %within% int] = i
    
    int = interval(m.dfs[m.dfs$id==i,]$start-1200,m.dfs[m.dfs$id==i,]$end+1200)
    chat$id_w_breaks[chat$time_msc %within% int] = i
    
    int = interval(dft.dfs[dft.dfs$id==i,]$dft.start,dft.dfs[dft.dfs$id==i,]$dft.end)
    chat$is_picks[chat$time_msc %within% int] = i
    team_figths = filter(tf.dfs,id == i)
    for (t in (1:nrow(team_figths))){
      tf = team_figths[t,]
      int = interval(tf$tf.start,tf$tf.end)
      chat$is_teamfight[chat$time_msc %within% int] = t 
    }
    print(i)
    }

table(chat$id_w_breaks)

chat = left_join(chat,e.dfs)
chat = left_join(chat,k.dfs)
chat = left_join(chat,dft.dtl.dfs)
chat = select(chat,-active_team,-extra_time,-total_time_taken,-order)

save(chat,file="~/chats/data/chat_with_meta.rda")
