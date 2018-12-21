

library(tidyverse)
library(ggthemes)

## Load Team data
poch_start <- as.Date('2014-07-01')

tottenham <- read_csv("http://api.clubelo.com/tottenham") %>% 
  filter(From >= poch_start) 
chelsea <- read_csv("http://api.clubelo.com/chelsea") %>% 
  filter(From >= poch_start)
arsenal <- read_csv("http://api.clubelo.com/arsenal") %>% 
  filter(From >= poch_start)
westham <-read_csv("http://api.clubelo.com/westham") %>% 
  filter(From >= poch_start) 
palace <- read_csv("http://api.clubelo.com/crystalpalace") %>% 
  filter(From >= poch_start) 

## Assign  Some Variables
season1 <- as.Date('2014-08-16')
season2 <- as.Date('2015-08-08')
season3 <- as.Date('2016-08-13')
season4 <- as.Date('2017-08-13')
season5 <- as.Date('2018-08-11')

seasons <- c(season1, season2, season3, season4,season5)

###Order Alphabetically
club_colors <- c("#ef0107", "#DBA111","#1B458F", "#132257", "#7A263A")

combined_teams <- bind_rows(arsenal, chelsea, palace, tottenham, westham)

start_labels <-combined_teams[!duplicated(combined_teams$Club),] %>% 
  mutate(x_position = as.Date("2014-05-01"))

end_labels <- combined_teams %>% 
  arrange(desc(From))
end_labels <- end_labels[!duplicated(end_labels$Club),] %>% 
  mutate(x_position = To + 90)
  


## Plot
ggplot(combined_teams, aes(x = To, y = Elo, color = Club)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = club_colors) +
  scale_x_date(breaks=seasons,  date_labels="%Y-%B") +
  geom_text(data = start_labels, aes(x = x_position, y = Elo, label = Club)) +
  geom_text(data = end_labels, aes(x = x_position, y = Elo, label = Club)) +
  labs(y = NULL,
       x = NULL,
       title = "The Pochettino Effect: Tottenham takes over London following Mauricio Pochettino's arrival",
       subtitle = "Club performance based on Elo rating system (clubElo.com)",
       caption = "Concept based on work of @jburnmurdoch")+ 
  theme_fivethirtyeight()


  
 
###Inspiration for project taken from John Burn-Murdoch TWitter: @jburnmurdoch####
 From https://gist.github.com/johnburnmurdoch/1b3f32aaf7757733bd68a6513ab86226 
jose.porto <- read_csv("http://api.clubelo.com/porto")
jose.chelsea <- read_csv("http://api.clubelo.com/chelsea")
jose.inter <- read_csv("http://api.clubelo.com/inter")
jose.real <- read_csv("http://api.clubelo.com/realmadrid")
jose.mufc <- read_csv("http://api.clubelo.com/manunited")

jose.all <- bind_rows(
  jose.porto %>% filter(From >= as.Date("2002-01-22") & To <= as.Date("2004-06-30")),
  jose.chelsea %>% 
    filter(
      (From >= as.Date("2004-07-01") & To <= as.Date("2007-09-19"))
    ),
  jose.chelsea %>% 
    filter(
      (From >= as.Date("2013-07-01") & To <= as.Date("2015-12-17"))
    ) %>% mutate(Club = "Chelsea 2"),
  jose.inter %>% filter(From >= as.Date("2008-07-01") & To <= as.Date("2010-06-30")),
  jose.real %>% filter(From >= as.Date("2010-07-01") & To <= as.Date("2013-06-30")),
  jose.mufc %>% filter(From >= as.Date("2016-07-01") & To <= as.Date("2018-12-18"))
)

jose.labels <- jose.all %>%
  group_by(Club) %>%
  top_n(1, Elo)

ggplot(jose.all, aes(To, Elo, group=Club, color=Club)) +
  theme_minimal() +
  geom_line() +
  geom_text(data=jose.labels, aes(y = Elo+10, label=Club)) +
  scale_color_discrete(guide=F) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
  labs(x = "", y = "", subtitle = "Team performance level (Elo rating)", title = "Mourinho’s recent stints have been marked by steep declines in performance after promising starts")

