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

df_roof <- V4_T2.12

View(df_roof)

# Table 1 for National Analysis
table_1 <- df_roof[1:3,]
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
  pivot_longer(c(grass_twigs:shingles), 
               names_to = "roof_type", values_to = "percentage") %>%
  mutate(roof_type = ifelse(roof_type == "grass_twigs", "Grass/Twigs",
                     ifelse(roof_type == "makuti_thatch", "Makuti",
                     ifelse(roof_type == "dung_mud", "Dung/Mud",
                     ifelse(roof_type == "ironsheets", "Iron Sheets",
                     ifelse(roof_type == "tincans", "Tin Cans",
                     ifelse(roof_type == "asbestos_sheets", "Asbestos ",
                     ifelse(roof_type == "concrete_cement", "Concrete",
                     ifelse(roof_type == "tiles", "Tiles",
                     ifelse(roof_type == "canvas_tents", "Canvas/Tents",
                     ifelse(roof_type == "decra_versatile", "Decra",
                     ifelse(roof_type == "nylon_cartons_cardboard", "Nyl_Cart_Card",
                     ifelse(roof_type == "shingles", "Shingles", roof_type))))))))))))) 
  
table_1_select_tidy

table_1_select_tidy$roof_type

# National

table_1_select_tidy_national <- table_1_select_tidy %>%
  filter(sub_county == "KENYA")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_national, 
       aes(area = percentage, fill = roof_type, 
           label = roof_type)) +
  geom_treemap() +
  labs(caption = "Visualization @willyokech | Source:rKenyaCensus") +
  labs(fill = "Roof Type") +
  theme(legend.position = "bottom") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10,
                    grow = TRUE) + 
  scale_fill_brewer(palette = "Spectral")

ggsave("images/building_materials_kenya_roof_national/national_treemap.png", width = 12, height = 8)

# Urban

table_1_select_tidy_urban <- table_1_select_tidy %>%
  filter(sub_county == "URBAN")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_urban, 
       aes(area = percentage, fill = roof_type, 
           label = paste(roof_type, " (", percentage,"%",")", sep =""))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10) 

ggsave("images/building_materials_kenya_roof_national/urban_treemap.png", width = 12, height = 8)


# Rural

table_1_select_tidy_rural <- table_1_select_tidy %>%
  filter(sub_county == "RURAL")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_rural, 
       aes(area = percentage, fill = roof_type, 
           label = paste(roof_type, " (", percentage,"%",")", sep =""))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10) 

ggsave("images/building_materials_kenya_roof_national/rural_treemap.png", width = 12, height = 8)
