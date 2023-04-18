# Mode of acquisition for the houses that have been purchased
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_floor <- V4_T2.14

View(df_floor)

# Table 1 for National Analysis
table_1 <- df_floor[1:3,]
View(table_1)

glimpse(table_1)

table_1 <- table_1 %>%
  clean_names()

table_1_select <- table_1 %>%
  select(-c(conventional_households, admin_area, not_stated))

View(table_1_select)
glimpse(table_1_select)


# 5) ggplot2 visualization

# Treemap

#install.packages("treemapify")
library(treemapify)

table_1_select_tidy <- table_1_select %>%
  pivot_longer(c(earth_sand:wallto_wall_carpet), 
               names_to = "floor_type", values_to = "percentage") %>%
  mutate(floor_type = ifelse(floor_type == "earth_sand", "Earth/Sand",
                            ifelse(floor_type == "dung", "Dung",
                                   ifelse(floor_type == "woodplunks_shingles_timber", "Timber",
                                          ifelse(floor_type == "palm_bamboo", "Palm/Bamboo",
                                                 ifelse(floor_type == "parquet_published_wood", "Wood Tiles",
                                                       ifelse(floor_type == "vinyl_asphaltstrips", "Vinyl/Asphalt",
                                                              ifelse(floor_type == "ceramictiles", "Tiles",
                                                                     ifelse(floor_type == "concrete_cement_terrazo", "Concrete/Terrazo",
                                                                            ifelse(floor_type == "wallto_wall_carpet", "Carpet", floor_type)))))))))) 

table_1_select_tidy

table_1_select_tidy$floor_type

# National

table_1_select_tidy_national <- table_1_select_tidy %>%
  filter(sub_county == "KENYA")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_national, 
       aes(area = percentage, fill = floor_type, 
           label = paste(floor_type, " (", percentage,"%",")", sep =""))) +
  geom_treemap() +
  labs(fill = "Floor Type") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10) 

ggsave("images/building_materials_kenya_floor_national/national_treemap.png", width = 12, height = 8)

# Urban

table_1_select_tidy_urban <- table_1_select_tidy %>%
  filter(sub_county == "URBAN")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_urban, 
       aes(area = percentage, fill = floor_type, 
           label = paste(floor_type, " (", percentage,"%",")", sep =""))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10) 

ggsave("images/building_materials_kenya_floor_national/urban_treemap.png", width = 12, height = 8)


# Rural

table_1_select_tidy_rural <- table_1_select_tidy %>%
  filter(sub_county == "RURAL")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_rural, 
       aes(area = percentage, fill = floor_type, 
           label = paste(floor_type, " (", percentage,"%",")", sep =""))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10) 

ggsave("images/building_materials_kenya_floor_national/rural_treemap.png", width = 12, height = 8)

