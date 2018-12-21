# By: Matt Leary
# Date: Dec 20, 2018
# Intent: Look at THFC perforamnce, comparted to other London based teams,
#        since Pochettino installed as manager
# Note: Inspiration for project taken from John Burn-Murdoch TWitter: @jburnmurdoch####
# From https://gist.github.com/johnburnmurdoch/1b3f32aaf7757733bd68a6513ab86226 


library(tidyverse)
library(ggthemes)

## Load Team data
poch_start <- as.Date('2014-05-27')
current_date <- today()

tottenham <- read_csv("http://api.clubelo.com/tottenham") %>% 
  filter(From >= poch_start, To <= current_date) 
chelsea <- read_csv("http://api.clubelo.com/chelsea") %>% 
  filter(From >= poch_start, To <= current_date) 
arsenal <- read_csv("http://api.clubelo.com/arsenal") %>% 
  filter(From >= poch_start, To <= current_date) 
# westham <-read_csv("http://api.clubelo.com/westham") %>% 
#   filter(From >= poch_start, To <= current_date) 
# palace <- read_csv("http://api.clubelo.com/crystalpalace") %>% 
#   filter(From >= poch_start, To <= current_date) 

## Assign  Some Variables
season1 <- as.Date('2014-08-16')
season2 <- as.Date('2015-08-08')
season3 <- as.Date('2016-08-13')
season4 <- as.Date('2017-08-13')
season5 <- as.Date('2018-08-11')

seasons <- c(season1, season2, season3, season4,season5)

###Order Alphabetically ()
#club_colors <- c("#ef0107", "#DBA111","#1B458F", "#132257", "#7A263A")
club_colors <- c("#ef0107", "#DBA111", "#132257")


#combined_teams <- bind_rows(arsenal, chelsea, palace, tottenham, westham)
combined_teams <- bind_rows(arsenal, chelsea, tottenham)


start_labels <-combined_teams[!duplicated(combined_teams$Club),] %>% 
  mutate(x_position = as.Date("2014-05-01"))

end_labels <- combined_teams %>% 
  arrange(desc(From))
end_labels <- end_labels[!duplicated(end_labels$Club),] %>% 
  mutate(x_position = To + 90)
  


## Plot
ggplot(combined_teams, aes(x = To, y = Elo, color = Club)) +
  geom_line(size = 1.5) +
  geom_text(data = start_labels, aes(x = x_position, y = Elo, label = Club, size = 2.75)) +
  geom_text(data = end_labels, aes(x = x_position, y = Elo, label = Club, size =  2.75)) +
  scale_color_manual(values = club_colors) +
  scale_x_date(breaks=seasons,  date_labels="%Y-%B") +
  labs(y = NULL,
       x = NULL,
       title = "The Pochettino Effect: Tottenham's performance since Mauricio Pochettino's arrival in May, 2014",
       subtitle = "Club performance based on Elo rating system (clubelo.com)") + 
  theme_fivethirtyeight() +
  theme(legend.position = "none")


  
 
