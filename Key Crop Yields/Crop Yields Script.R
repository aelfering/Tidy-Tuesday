
####  Loading the libraries and data  ####
library(dplyr)
library(ggplot2)
library(tidyverse)
#library(tidylog)
library(viridis)
library(sf)
library(raster)
#library(dplyr)
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

avoid_entity_names <- key_crop_yields %>%
  filter(is.na(Code) | Entity == 'World') %>%
  distinct(Entity)

# long pivot key crop yield df
key_crop_percentiles <- key_crop_yields %>%
  filter(Year == max(Year)) %>%
  anti_join(avoid_entity_names) %>%
  group_by(Entity,
           Code,
           Year) %>%
  #pivot_longer()
  pivot_longer(cols = -c('Entity',
                         'Code',
                         'Year'),
               names_to = 'Crop') %>%
  ungroup() %>%
  group_by(Crop) %>%
  mutate(value = ifelse(is.na(value), 0, value),
         Crop = gsub(' \\(tonnes per hectare\\)', '', Crop)) %>%
  filter(Crop == 'Cocoa beans')

head(key_crop_percentiles)

entity_country_name_fixes <- key_crop_percentiles %>%
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

mark1 <- full_join(entity_country_name_fixes,
                   worlddf,
                   by = c('Entity' = 'name_long'))

mark2 <- mark1 %>%
  mutate(group1 = case_when(Crop_Percentile <= 0.25 ~ '0-25th Percentile',
                            Crop_Percentile > 0.25 & Crop_Percentile <= 0.50 ~ '25-50th',
                            Crop_Percentile > 0.50 & Crop_Percentile <= 0.75 ~ '50-75th',
                            Crop_Percentile > 0.75 ~ '75th-100th'))
  

ggplot(subset(mark2, Crop %in% c('Cocoa beans'))) +
  geom_sf(aes(geometry = geom, 
              fill = group1),
          color = 'white',
          size = 0.2) +
  # This extra plot highlights countries with highest percentages
  geom_sf(data = subset(mark2, Crop %in% c('Cocoa beans') & Crop_Percentile >= 0.9),
          mapping = aes(geometry = geom,
                        fill = group1),
          color = 'black',
          size = 0.5) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  scale_fill_manual(values = c('#ffc6a7', '#f58d55', '#d25b22', '#a72c00')) +
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




