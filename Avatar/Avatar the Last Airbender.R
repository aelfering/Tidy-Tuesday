#### Load the packages and data source  ####

library(tidyverse)
library(dplyr)
library(tidytext)
library(viridis)

dat <- appa::appa

####  Data Cleaning ####

# Generating a dataframe of all episodes with row index
episodes_full <- dat %>%
  distinct(book_num,
           chapter_num) %>%
  mutate(row = row_number())

# Who are the top 15 characters to make appearances?
top_characters <- dat %>%
  filter(character != 'Scene Description') %>%
  distinct(character, 
           book_num, 
           chapter_num) %>%
  unite(Season_Episode, c('book_num', 'chapter_num'), sep = '-') %>%
  group_by(character) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(row_number() <= 15) %>%
  select(character)

# What are the most common words for each character?
character_word_freq <- dat %>%
  filter(character != 'Scene Description') %>%
  unnest_tokens(word, 
                character_words) %>%
  group_by(character, 
           book_num,
           chapter_num,
           word) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  anti_join(stop_words) %>%
  arrange(character, desc(n))

# How many total words spoken by character?
total_character_words <- character_word_freq %>%
  group_by(character,
           book_num,
           chapter_num) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  group_by(character) %>%
  mutate(total_n = sum(n)) %>%
  ungroup()

total_character_words %>%
  distinct(character,
           total_n) %>%
  inner_join(top_characters, by = 'character') %>%
  arrange(desc(total_n))

# Bringing all of the dataframes together
final_df <- dat %>%
  filter(character != 'Scene Description') %>%
  distinct(character, 
           book_num, 
           chapter_num) %>%
  inner_join(episodes_full, by = c('book_num' = 'book_num', 'chapter_num' = 'chapter_num')) %>%
  inner_join(top_characters, by = 'character') %>%
  inner_join(total_character_words, by = c('book_num' = 'book_num', 'chapter_num' = 'chapter_num', 'character' = 'character')) %>%
  group_by(character) %>%
  complete(row = seq(1, 61, by = 1)) %>%
  mutate(sort_column = total_n,
         sort_column = ifelse(is.na(sort_column), 0, sort_column),
         sort_column = max(sort_column)) %>%
  ungroup() %>%
  as.data.frame()

####  Visualization ####

ggplot(final_df,
       aes(x = row,
           y = reorder(character, sort_column))) +
  geom_tile(color = '#e1e1e1',
            fill = NA,
            size = 0.5,
            alpha = 0.3) +
  geom_tile(data = subset(final_df, !is.na(n)),
            color = 'white',
            mapping = aes(x = row,
                          y = reorder(character, sort_column),
                          fill = n),
            size = 0.5) +
  #scale_y_discrete(limits=c(1,61),
  #                 breaks = seq(1, 61, by = 10)) +
  scale_fill_viridis_c(option = 'inferno',
                       breaks = c(50, 150, 250)) +
  labs(title = 'Which Character Appeared and Spoke the Most in Avatar: Last Air Bender?',
       x = 'Episode',
       y = '',
       caption = 'Visualization by Alex Elfering\nSource: Appa R Package',
       fill = 'Total Words Spoken') +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 18, 
                                  family = 'Arial'),
        legend.position = 'top',
        axis.text.x = element_text(vjust = 0.5, 
                                   hjust = 0, 
                                   size = 12),
        axis.title.y = element_text(hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        axis.title.x = element_text(hjust = 0.5, 
                                    vjust = 0.5, 
                                    size = 12),
        plot.subtitle = element_text(size = 15, 
                                     family = 'Arial'),
        plot.caption = element_text(size = 12, 
                                    family = 'Arial'),
        axis.title = element_text(size = 12, 
                                  family = 'Arial'),
        axis.text = element_text(size = 12, 
                                 family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, 
                                           hjust = 0, 
                                           face = 'bold', 
                                           color = 'brown', 
                                           family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()) 











