library(rjson)
library(stringr)
library(lubridate)
library(purrr)
library(magrittr)
library(dplyr)
library(readr)

### Getting ids
# Main Event
load("~/chats/data/primary_data/ti7.rda")
load("~/chats/data/series_df.rda")
ti7_me = select(series.df,id,channel)

ids_to_search = dplyr::filter(ti7_me,channel==1)$id 
# Quals
#teams_to_get = gs_title("TI7 STM 1309") %>% gs_read(1)
teams_to_get = read_csv("~/chats/data/teams_to_get.csv")

load("~/chats/data/series_info.rda")
ids2 = filter(ser.dfs, teams %in% teams_to_get$teams)$id
#Unite
ids_to_search = c(ids_to_search,ids2)
rm(teams_to_get,series.df,ser.dfs,ids2,ti7_me)

### Start getting match events

match.ids = ids_to_search
dft_missed = c()
tf_missed = c()
e_missed= c()

e.dfs = data.frame()
m.dfs = data.frame()
tf.dfs = data.frame()
k.dfs = data.frame()

dft.dtl.dfs = data.frame()
dft.dfs = data.frame()

#match.ids = match.ids[match.ids < 3367820228] # на одной айди апи остановило, эта строчка продолжает азгрузку

for (id in match.ids){
  print(id)
  req = str_c("https://api.opendota.com/api/matches/",id)
  json = fromJSON(file=req)
  
  # match information
  start = json$start_time %>%
    as.POSIXct(origin = "1970-01-01")
  duration =  json$duration
   
  # picks
    if (length(json$draft_timings) > 0){
    dft.dtl = json$draft_timings %>%
      map_df(magrittr::extract,c("order","pick","active_team","extra_time","total_time_taken"))
    dft.dtl$time_msc = start + cumsum(dft.dtl$total_time_taken)
    dft.dtl$id = id
    dft.dtl.dfs = rbind(dft.dtl.dfs,dft.dtl)
    dft.sum = data.frame(id=id,dft.start=min(dft.dtl$time_msc),dft.end=max(dft.dtl$time_msc))
    dft.dfs = rbind(dft.dfs,dft.sum)
    picks_duration = sum(dft.dtl$total_time_taken)
     } else dft_missed = c(dft_missed,id)
    
    # 60 - выбор героев
    # 60 - strategic time
    # 12 - запуск карты
    # 90 - гонг звучит
    # 7 - делей
  start = start + picks_duration + 60 + 60 + 12 + 90 
  end = start + json$duration
  matches = data.frame(id=id,start=start,end=end)
  m.dfs = rbind(m.dfs,matches)

  
  # team fights
  if (length(json$teamfights) > 0){
    tf.start = map(json$teamfights,"start") %>% flatten_chr() %>% as.numeric()
    tf.start = start + tf.start
    tf.end = map(json$teamfights,"end") %>% flatten_chr() %>% as.numeric()
    tf.end =  start + tf.end
    team.fights = data.frame(id=id,tf.start=tf.start,tf.end=tf.end)
    tf.dfs = rbind(tf.dfs,team.fights)
  } else tf_missed = c(tf_missed,id)
  
  # events
  if (length(json$objectives) > 0){
    events = json$objectives %>% map_df(magrittr::extract,c("time","type"))
    events$time = start + events$time
    names(events)[1] = "time_msc"
    events$id = id
    e.dfs = rbind(e.dfs,events)
  } else e_missed = c(e_missed,id)
  
  # deaths
  if (length(json$players %>% map("kills_log") %>% flatten()) > 0){
    kills = json$players %>% map("kills_log") %>% flatten() %>% map_chr("time") %>% as.numeric()
    kills = start + kills
    kills = data.frame(id=id,time_msc = kills)
    k.dfs = rbind(k.dfs,kills)
  }
  
  
  Sys.sleep(1.5)
}


e.dfs$id = as.numeric(e.dfs$id)
k.dfs$id = as.numeric(as.character(k.dfs$id))
dft.dtl.dfs$id = as.numeric(dft.dtl.dfs$id)
m.dfs$id = as.numeric(as.character(m.dfs$id))
dft.dfs$id = as.numeric(as.character(dft.dfs$id))
tf.dfs$id = as.numeric(as.character(tf.dfs$id))

# тут какая-то хня с k.dfs и m.dfs -- проверить

k.dfs$kill = 1
e.dfs$event = 1
k.dfs = k.dfs %>% group_by(id,time_msc) %>% count()
names(k.dfs)[3] = "kill"
e.dfs = e.dfs %>% group_by(id,time_msc) %>% count()
names(e.dfs)[3] = "event"

save(e.dfs, m.dfs, tf.dfs, k.dfs, dft.dfs,dft.dtl.dfs,e_missed,tf_missed,dft_missed,file='~/chats/data/events_ti7.rda')
