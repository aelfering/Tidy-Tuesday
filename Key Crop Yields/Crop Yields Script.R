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
# What is done:
#   Removed entity names found in the df above
#   pivoted the crop columns into one column with rows for each crop by country
#   cleaned the crop names to remove '(tonnes per hectare)'
#   filtered on cocoa beans
#   fixed names to join with world df
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
  filter(Crop == 'Potatoes') %>%
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
         Crop_Percentile = NA) %>%
  anti_join(country_names,
            by = c('name_long' = 'Entity')) %>%
  filter(iso_a2 != 'AQ')

na_countries <- key_crop_cleaned %>%
  dplyr::select(Entity,
                value) %>%
  filter(is.na(value)) %>%
  mutate(Crop_Percentile = NA) %>%
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

ggplot(full_country_df) +
  geom_sf(aes(geometry = geom, 
              fill = Crop_Percentile),
          color = 'white',
          size = 0.2) +
  geom_sf(data = subset(full_country_df, Crop_Percentile >= 0.9),
          mapping = aes(geometry = geom,
                        fill = Crop_Percentile),
          color = 'black',
          size = 0.5) +
  labs(title = 'Countries that Yield Cocoa Beans are Usually Closest to Equator',
       subtitle = 'Yields grouped by percentiles. Countries within the 90th Percentile are highlighted in black.',
       caption = 'Visualization by Alex Elfering\nSouce: Our World in Data',
       fill = 'Percentile:') +
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