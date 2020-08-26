#### Load the packages and data source  ####

library(tidyverse)
library(dplyr)
library(tidytext)
library(viridis)
library(ggforce)

dat <- appa::appa

####  Data Cleaning ####

# Generating a dataframe of all episodes with row index
episodes_full <- dat %>%
  distinct(book_num,
           chapter_num) %>%
  mutate(row = row_number())

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
  #anti_join(stop_words) %>%
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
  filter(row_number() <= 20) %>%
  select(character)

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
  as.data.frame() %>%
  replace(is.na(.), 0) %>%
  mutate(n_bucket = ifelse(n <= 250 & n > 1, '1-250',
                           ifelse(n > 250 & n <= 500, '250-500',
                                  ifelse(n > 500 & n <= 750, '500-750', 
                                         ifelse(n == 0, 'Did not speak/not featured', '+750 words')))))

final_df$n_bucket <- factor(final_df$n_bucket, levels = unique(c('Did not speak/not featured', '1-250', '250-500', '500-750', '+750 words')))

####  Visualization ####

ggplot(subset(final_df, row > 0),
       aes(x = row,
           y = reorder(character, sort_column))) +
  geom_tile(#color = '#e1e1e1',
            fill = NA,
            size = 0.5,
            alpha = 0.3) +
  geom_tile(data = subset(final_df, !is.na(n), row > 0),
            color = 'white',
            mapping = aes(x = row,
                          y = reorder(character, sort_column),
                          fill = n_bucket),
            size = 0.5) +
  scale_fill_manual(values = c('whitesmoke', '#ffcba7', '#ff935f', '#ef5824', '#d10000')) +
  labs(#title = 'Which Character Appeared and Spoke the Most in Avatar: Last Air Bender?',
  #     subtitle = 'Among the characters who appeared the most in ATLAB, Sokka spoke the most at +18k words by the end of Book Three. Aang spoke +17k\nwords, and Katara spoke just under 15k.',
       x = 'Episode',
       y = '',
  #     caption = 'Visualization by Alex Elfering\nSource: Appa R Package',
       fill = 'Total Words Spoken'
    ) +
  geom_mark_rect(data = subset(final_df, row == 27), 
                 aes(group = row),
                 radius = 0, expand = 0.008, size = 0.5, fill = NA) +
  geom_vline(xintercept = 0.5, size = 0.5) +
  geom_vline(xintercept = 20.5, size = 0.5) +
  geom_vline(xintercept = 40.5, size = 0.5) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'none',
        axis.text.x = element_text(vjust = 0.5, hjust = 0, size = 12),
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        axis.title.x = element_text(hjust = 0.5, vjust = 0.5, size = 12),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()) 











