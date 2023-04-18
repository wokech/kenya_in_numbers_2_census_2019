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

df_wall <- V4_T2.13

View(df_wall)


# Table 1 for National Analysis
table_1 <- df_wall[1:3,]
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
  pivot_longer(c(cane_palm_trunks:prefabricated_panels), 
               names_to = "wall_type", values_to = "percentage") %>%
  mutate(wall_type = ifelse(wall_type == "cane_palm_trunks", "Cane/Palm",
                     ifelse(wall_type == "grass_reeds", "Grass/Reeds",
                     ifelse(wall_type == "mud_cowdung", "Mud/Cowdung",
                     ifelse(wall_type == "stonewithmud", "Stone and Mud",
                     ifelse(wall_type == "coveredadobe", "Covered Adobe",
                     ifelse(wall_type == "uncoveredadobe", "Uncovered Adobe",
                     ifelse(wall_type == "plywood_cardboard", "Plywood/Cardboard",
                     ifelse(wall_type == "offcuts_reused_wood_woodplanks", "Offcuts/Wood Pieces",
                     ifelse(wall_type == "ironsheets", "Iron Sheets",
                     ifelse(wall_type == "concrete_concreteblocks_precastwall", "Concrete",
                     ifelse(wall_type == "stonewithlime_cement", "Limestone and Cement",
                     ifelse(wall_type == "bricks", "Bricks",
                     ifelse(wall_type == "canvas_tents", "Canvas/Tent",
                     ifelse(wall_type == "nylon_cartons", "Nylon/Cartons",
                     ifelse(wall_type == "timber", "Timber",
                     ifelse(wall_type == "prefabricated_panels", "Prefab Panels", wall_type))))))))))))))))) 

table_1_select_tidy

table_1_select_tidy$wall_type

# National

table_1_select_tidy_national <- table_1_select_tidy %>%
  filter(sub_county == "KENYA")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_national, 
       aes(area = percentage, fill = wall_type, 
           label = paste(wall_type, " (", percentage,"%",")", sep =""))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10) 

ggsave("images/building_materials_kenya_wall_national/national_treemap.png", width = 12, height = 8)

# Urban

table_1_select_tidy_urban <- table_1_select_tidy %>%
  filter(sub_county == "URBAN")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_urban, 
       aes(area = percentage, fill = wall_type, 
           label = paste(wall_type, " (", percentage,"%",")", sep =""))) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10) 

ggsave("images/building_materials_kenya_wall_national/urban_treemap.png", width = 12, height = 8)


# Rural

table_1_select_tidy_rural <- table_1_select_tidy %>%
  filter(sub_county == "RURAL")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_rural, 
       aes(area = percentage, fill = wall_type, 
           label = paste(wall_type, " (", percentage,"%",")", sep =""))) +
  geom_treemap() +
  labs(fill = "Wall Type") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10) 

ggsave("images/building_materials_kenya_wall_national/rural_treemap.png", width = 12, height = 8)

