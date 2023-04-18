# What is the distribution of pastoralist livestock in Kenya
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
library(patchwork)

### To filter out the FOREST data
### filter_all(any_vars(grepl("FOREST", .)))
###

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_livestock <- V4_T2.24

View(df_livestock)

# Table 1 for County and SubCounty Analysis
table_1_pasto <- df_livestock[2:393,]
View(table_1_pasto)

glimpse(table_1_pasto)

table_1_pasto <- table_1_pasto %>%
  clean_names()

table_1_pasto_select <- table_1_pasto %>%
  select(county, sub_county, admin_area, farming, sheep, goats, indigenous_cattle) %>%
  mutate(total_pasto_livestock = sheep + goats + indigenous_cattle) %>%
  mutate(ind_cattle_farm_household = round(indigenous_cattle/farming)) %>%
  mutate(goats_farm_household = round(goats/farming)) %>%
  mutate(sheep_farm_household = round(sheep/farming)) %>%
  mutate(total_pasto_farm_household = round(total_pasto_livestock/farming))

View(table_1_pasto_select)
glimpse(table_1_pasto_select)

table_1_pasto_select_subcounty <- table_1_pasto_select %>%
  filter(admin_area == "SubCounty")

# Top 10 subcounties for sheep, goats, and indigenous cows combined + Plot

table_1_pasto_select_subcounty_top10 <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, total_pasto_livestock) %>%
  unite("county_sub", sub_county, county, sep = ", ") %>%
  arrange(desc(total_pasto_livestock)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_subcounty_top10)

table_1_pasto_select_subcounty_top10_plot <- table_1_pasto_select_subcounty_top10 %>%
  ggplot(aes(x = reorder(county_sub, total_pasto_livestock), y = total_pasto_livestock)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "aquamarine3") + 
  coord_flip() + 
  theme_classic()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "County", 
       y = "Number of Livestock", 
       title = "Pastoral Livestock",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

table_1_pasto_select_subcounty_top10_plot

ggsave("images/livestock_kenya_subcounty_pasto/subcounty_top10_pasto.png", width = 6, height = 4)

# Top 10 subcounties for sheep + Plot

table_1_pasto_select_subcounty_sheep_top10 <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, sheep) %>%
  unite("county_sub", sub_county, county, sep = ", ") %>%
  arrange(desc(sheep)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_subcounty_sheep_top10)

table_1_pasto_select_subcounty_sheep_top10_plot <- table_1_pasto_select_subcounty_sheep_top10 %>%
  ggplot(aes(x = reorder(county_sub, sheep), y = sheep)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "brown3") + 
  coord_flip() + 
  theme_classic()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "County", 
       y = "Number of Livestock", 
       title = "Sheep",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

table_1_pasto_select_subcounty_sheep_top10_plot

ggsave("images/livestock_kenya_subcounty_pasto/subcounty_top10_sheep.png", width = 6, height = 4)

# Top 10 subcounties for goats + Plot

table_1_pasto_select_subcounty_goats_top10 <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, goats) %>%
  unite("county_sub", sub_county, county, sep = ", ") %>%
  arrange(desc(goats)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_subcounty_goats_top10)

table_1_pasto_select_subcounty_goats_top10_plot <- table_1_pasto_select_subcounty_goats_top10 %>%
  ggplot(aes(x = reorder(county_sub, goats), y = goats)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkgoldenrod1") + 
  coord_flip() + 
  theme_classic()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "County", 
       y = "Number of Livestock", 
       title = "Goats",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

table_1_pasto_select_subcounty_goats_top10_plot

ggsave("images/livestock_kenya_subcounty_pasto/subcounty_top10_goats.png", width = 6, height = 4)

# Top 10 subcounties for indigenous cows + Plot

table_1_pasto_select_county_indi_cow_top10 <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, indigenous_cattle) %>%
  unite("county_sub", sub_county, county, sep = ", ") %>%
  arrange(desc(indigenous_cattle)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_county_indi_cow_top10)

table_1_pasto_select_county_indi_cow_top10_plot <- table_1_pasto_select_county_indi_cow_top10 %>%
  ggplot(aes(x = reorder(county_sub, indigenous_cattle), y = indigenous_cattle)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "deeppink4") + 
  coord_flip() + 
  theme_classic()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "County", 
       y = "Number of Livestock", 
       title = "Indigenous Cattle",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

table_1_pasto_select_county_indi_cow_top10_plot

ggsave("images/livestock_kenya_subcounty_pasto/subcounty_top10_indi_cow.png", width = 6, height = 4)

