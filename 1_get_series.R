library(rvest)
library(tidyverse)
library(tidytext)

url_start = "https://www.dotabuff.com/esports/leagues/"
url_mid = "/series?original_slug="
url_end = "&page="
league_to_search = "5401-the-international-2017"

n_series = 393
n_pages = ceiling(n_series / 20)
last_page = 20 - (n_pages*20 - n_series)

ser.dfs = data.frame()
for (i in 1:n_pages){
  url = str_c(url_start,league_to_search,url_mid,league_to_search,url_end,i)
  page = read_html(url)
  
  stage = page %>% html_nodes(xpath="//div[1]/div[8]/div[2]/div[3]/section[2]/div/article/div[1]/table/tbody/tr/td[2]/div[3]/small/a/span
  ") %>% html_text()

  
  team_one = page %>% html_nodes(xpath="//td[@class='series-teams']/div[1]/span[2]") %>%
    html_text() 
  
  team_two = page %>% html_nodes(xpath="//td[@class='series-teams']/div[2]/span[2]") %>%
    html_text()
  teams = str_c(team_one," vs ",team_two)
  
  games_ids = page %>% html_nodes(xpath="//td[@class='series-game-icons r-none-mobile']//span//a") %>%
    html_attr("href") %>% extract_numeric()
  
  games = page %>% html_nodes(xpath="//td[@class='series-game-icons r-none-mobile']//span//a") %>%
    html_text()
  
  
  if (i == n_pages) {ids = 1:last_page} else {ids = 1:20}
  num = c(diff(which(games==1)),length(games) - sum(diff(which(games==1))))
  ids = rep(ids,num)
  games = data.frame(game = ids,id=games_ids) %>% group_by(game) %>% summarise(ids = paste0(id,collapse=' '))
  series = data.frame(stage=stage,teams = teams, ids = games$ids)
  
  ser.dfs = rbind(ser.dfs,series)
  Sys.sleep(1)
  print(i)
}

ser.dfs$ids = as.character(ser.dfs$ids)
ser.dfs$teams = as.character(ser.dfs$teams)
ser.dfs$stage = as.character(ser.dfs$stage)
ser.dfs$stage[str_detect(ser.dfs$stage,fixed("Qualifier"))] = "TI7 Qualifier"

ser.dfs = ser.dfs %>%
  unnest_tokens(id,ids)

table(ser.dfs$stage)

save(ser.dfs,file="~/shared/dota2-research/twitch/chat_data/ti7/data/series_info.rda")
