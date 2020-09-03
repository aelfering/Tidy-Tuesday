####  Loading the libraries and data  ####
library(dplyr)
library(ggplot2)
library(tidyverse)
#library(tidylog)
library(viridis)
library(sf)
library(raster)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(readr)

# Key Crop Yields
key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

# World data frame 
worlddf <- world

####  Data Cleaning and Joining ####

# these are entity names that I should avoid running calc on
avoid_entity_names <- key_crop_yields %>%
  filter(is.na(Code) | Entity == 'World') %>%
  distinct(Entity)

# this cleans the key_crop_yields df
key_crop_cleaned <- key_crop_yields %>%
  filter(Year == max(Year)) %>%
  anti_join(avoid_entity_names) %>%
  group_by(Entity,
           Code,
           Year) %>%
  pivot_longer(cols = -c('Entity',
                         'Code',
                         'Year'),
               names_to = 'Crop') %>%
  ungroup() %>%
  mutate(Crop = gsub(' \\(tonnes per hectare\\)', '', Crop)) %>%
  filter(Crop == 'Bananas') %>%
  mutate(Entity = case_when(Entity == 'Democratic Republic of Congo' ~ 'Democratic Republic of the Congo',
                            Entity == 'Russia' ~ 'Russian Federation',
                            Entity == 'Timor' ~ 'Timor-Leste',
                            Entity == "Cote d'Ivoire" ~ "Côte d'Ivoire",
                            Entity == 'Congo' ~ 'Republic of the Congo',
                            Entity == 'Swaziland' ~ 'eSwatini',
                            Entity == 'Gambia' ~ 'The Gambia',
                            Entity == 'Laos' ~ 'Lao PDR',
                            Entity == 'North Korea' ~ 'Dem. Rep. Korea',
                            Entity == 'South Korea' ~ 'Republic of Korea',
                            Entity == 'Brunei' ~ 'Brunei Darussalam',
                            TRUE ~ Entity))

# What countries are not on this list?
country_names <- dplyr::select(key_crop_cleaned, Entity)

null_countries <- worlddf %>%
  mutate(value = NA,
         Crop_Percentile = 0) %>%
  anti_join(country_names,
            by = c('name_long' = 'Entity')) %>%
  filter(iso_a2 != 'AQ')

na_countries <- key_crop_cleaned %>%
  dplyr::select(Entity,
                value) %>%
  filter(is.na(value)) %>%
  mutate(Crop_Percentile = 0) %>%
  inner_join(world,
             by = c('Entity' = 'name_long'))

na_null_countries <- bind_rows(null_countries, na_countries)

percentile_countries <- key_crop_cleaned %>%
  dplyr::select(Entity,
                value) %>%
  filter(!is.na(value)) %>%
  mutate(Crop_Percentile = ntile(value, 100)/100) %>%
  inner_join(world,
             by = c('Entity' = 'name_long'))

full_country_df <- bind_rows(percentile_countries, na_null_countries)
       
full_country_buckets <- full_country_df %>%
  mutate(Buckets = ifelse(Crop_Percentile == 0, 'Did not Produce',
                          ifelse(Crop_Percentile > 0 & Crop_Percentile <= 0.25, '1-25th',
                                 ifelse(Crop_Percentile > 0.25 & Crop_Percentile <= 0.5, '25-50th',
                                        ifelse(Crop_Percentile > 0.5 & Crop_Percentile <= 0.75, '50-75th',
                                               ifelse(Crop_Percentile > 0.75 & Crop_Percentile <= 1, '75-100th', NA))))))

full_country_buckets$Buckets <- factor(full_country_buckets$Buckets, levels = unique(c('Did not Produce', '1-25th', '25-50th', '50-75th', '75-100th')))

####  Visualization ####
ggplot(full_country_buckets) +
  geom_sf(aes(geometry = geom, 
              fill = Buckets),
          color = 'white',
          size = 0.2) +
  geom_sf(data = subset(full_country_buckets, Crop_Percentile >= 0.9),
          mapping = aes(geometry = geom,
                        fill = Buckets),
          color = 'black',
          size = 0.5) +
  labs(title = 'Which Countries Yield the Most Bananas?',
       subtitle = 'Yields grouped by percentiles. Countries within the 90th Percentile are highlighted in black.',
       caption = 'Visualization by Alex Elfering\nSouce: Our World in Data',
       fill = 'Percentile:') +
  scale_fill_manual(values = c('whitesmoke', '#ffcba7', '#fb9567', '#e95d33', '#d10000')) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 14, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()
  ) 