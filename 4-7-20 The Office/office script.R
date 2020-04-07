library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

office_chg <- office_ratings %>%
  arrange(air_date) %>%
  mutate(season_ep = paste(season, episode, sep = '-'),
         season_label = paste("Season", season, sep = ' '),
         overall_episode = row_number()) %>%
  group_by(season) %>%
  mutate(Median.Rating = median(imdb_rating),
         Avg.Rating = mean(imdb_rating))

season_breaks <- office_chg %>%
  group_by(season) %>%
  slice(which.max(overall_episode)) %>%
  mutate(ID = overall_episode + 0.5) %>%
  select(ID)
  
best_episode_ratings <- office_chg %>%
  mutate(Season_Index = imdb_rating - Avg.Rating) %>%
  group_by(season) %>%
  slice(which.max(Season_Index)) %>%
  ungroup() %>%
  mutate(Index.Label = paste('+', 
                             round(Season_Index, 
                                   1), 
                             sep = ''),
         title.Season = paste(title, ' (', season_label, ')', sep = ''))

ggplot(best_episode_ratings,
       aes(x = reorder(title.Season, 
                       -season),
           y = Avg.Rating)) +
  coord_flip() +
  geom_segment(aes(x = reorder(title.Season,
                               -season), 
                   xend = reorder(title.Season,
                                  -season), 
                   y = Avg.Rating, 
                   yend = imdb_rating),
               size = 1,
               color = 'gray') +
  geom_point(color = '#73a2c6',
             size = 12) +
  geom_point(data = best_episode_ratings,
             mapping = aes(x = title.Season,
                           y = imdb_rating),
             color = '#00429d',
             size = 12) +
  geom_text(data = best_episode_ratings,
            mapping = aes(x = title.Season,
                          y = imdb_rating,
                          label = Index.Label),
            hjust = 0.5,
            vjust = 0.5,
            color = 'white') +
  labs(title = "Which Episodes from The Office Rated the Best?",
       subtitle = "Among episodes from The Office that received the best IMDd ratings in their respective\nseasons, users rated 'Goodbye, Michael' and 'The Finale' the best respective to their seasons.",
       x = '',
       y = 'IMDd Rating',
       caption = 'Visualization by Alex Elfering\nSource: The schrute R package') +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 18, 
                                  family = 'Arial'),
        plot.subtitle = element_text(size = 15, 
                                     family = 'Arial'),
        plot.caption = element_text(size = 12, 
                                    family = 'Arial'),
        axis.title = element_text(size = 12, 
                                  family = 'Arial'),
        axis.text = element_text(size = 12, 
                                 family = 'Arial'),
        strip.text = ggplot2::element_text(size = 22, 
                                           hjust = 0),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", 
                                 linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 

ggplot(office_chg, 
       aes(x = overall_episode,
           y = imdb_rating)) +
  geom_vline(season_breaks,
             mapping = aes(xintercept = ID),
             color = "#cbcbcb") +
  geom_smooth(#method = "lm",
    aes(group = season),
    color = '#3e897d',
    se = F) +
  geom_point(color = '#a0ddc7',
             size = 3,
             alpha = 0.5) +
  labs(title = 'The Ratings for the Office Declined Over Time',
       subtitle = 'IMDb ratings between Season 1 and 9',
       x = 'Episodes',
       y = 'IMDb Ratings',
       caption = 'Visualization by Alex Elfering\nSource: The schrute R package') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 22, hjust = 0),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.x = element_blank()) 


