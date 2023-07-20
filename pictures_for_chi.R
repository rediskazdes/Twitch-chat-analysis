library(ggplot2)
library(tidyverse)
library(tidytext)
library(ineq)

load("/srv/store/students/bulygind/chats/data/stats_wo24.rda")

ginis_view_stats = left_join(ginis_view_stats,select(series.df,id,duration))

main_color = "#6F01A8"
sec_color = "#FFEB74"
my_theme <- function() {
  theme(panel.grid.minor = element_blank(),
        #panel.grid.major.x = element_line(colour = "orange"),
        panel.grid.major.y = element_line(colour = sec_color),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(color=main_color,size=24,family = 'AvantGarde'),
        axis.text.y = element_text(color=main_color,size=24,family = 'AvantGarde'),
        axis.title.x = element_text(color=main_color,size=30,family='AvantGarde'),
        axis.title.y = element_text(color=main_color,size=30,family='AvantGarde')
        
        )
   }


ginis_view_stats %>% ggplot() + geom_boxplot(aes(stage,gini),color=main_color) +
  my_theme() + ylab("Gini Index") + xlab("Tournament Stage")


my_theme2 <- function() {
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = sec_color),
        panel.grid.major.y = element_line(colour = sec_color),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(color=main_color,size=24,family = 'AvantGarde'),
        axis.text.y = element_text(color=main_color,size=24,family = 'AvantGarde'),
        axis.title.x = element_text(color=main_color,size=30,family='AvantGarde'),
        axis.title.y = element_text(color=main_color,size=30,family='AvantGarde')
        
  )
}


ginis_view_stats %>% ggplot() + geom_point(aes(n_views,gini,color=stage)) +
  my_theme2() + ylab("Gini Index") + xlab("Number of chat participants") +
  geom_smooth(aes(n_views,gini),color=main_color)

ginis_view_stats %>% ggplot() + geom_point(aes(n_views,av_msgs,color=stage)) +
  my_theme2() + ylab("Av. # of msgs per user") + xlab("Number of chat participants") +
  geom_smooth(aes(n_views,av_msgs),color=main_color)

ginis_view_stats %>% ggplot() + geom_point(aes(n_views,gini_vws,color=stage)) +
  my_theme2() + ylab("Gini index of users activity") + xlab("Number of chat participants") +
  geom_smooth(aes(n_views,gini_vws),color=main_color)

ginis_view_stats %>% ggplot() + geom_point(aes(n_views,sh,color=stage)) +
  my_theme2() + ylab("Share of  emotes") + xlab("Number of chat participants") +
  geom_smooth(aes(n_views,sh),color=main_color)



