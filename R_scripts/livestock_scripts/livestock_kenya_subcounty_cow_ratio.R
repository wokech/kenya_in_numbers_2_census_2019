# What is the distribution of exotic and indigenous cattle in Kenya
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
  mutate(total_pasto_farm_household = round(total_pasto_livestock/farming)) %>%
  mutate(indi_exotic_ratio = round(indigenous_cattle/(exotic_cattle_dairy + exotic_cattle_beef),1)) %>%
  mutate(indi_exotic_ratio = round(indigenous_cattle/(exotic_cattle_dairy + exotic_cattle_beef),1))


View(table_1_pasto_select)
glimpse(table_1_pasto_select)

table_1_pasto_select_subcounty <- table_1_pasto_select %>%
  filter(admin_area == "SubCounty")

# Top 10 subcounties for the indi:exotic ratio + Plot

table_1_pasto_select_subcounty_top10_cow_ratio <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, indi_exotic_ratio) %>%
  unite("county_sub", sub_county, county, sep = ", ") %>%
  arrange(desc(indi_exotic_ratio)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_subcounty_top10_cow_ratio)

table_1_pasto_select_subcounty_top10_cow_ratio_plot <- table_1_pasto_select_subcounty_top10_cow_ratio %>%
  ggplot(aes(x = reorder(county_sub, indi_exotic_ratio), y = indi_exotic_ratio)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "goldenrod3") + 
  coord_flip() + 
  theme_classic()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "County", 
       y = "Ratio of indigenous to exotic cattle", 
       title = "Top 10",
       caption = "") +
  theme(axis.title.x =element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

table_1_pasto_select_subcounty_top10_cow_ratio_plot

ggsave("images/livestock_kenya_subcounty_pasto/subcounty_top10_cow_ratio.png", width = 6, height = 4)


     
# Bottom 10 subcounties for the indi:exotic ratio + Plot

table_1_pasto_select_subcounty_bottom10_cow_ratio <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, indi_exotic_ratio) %>%
  unite("county_sub", sub_county, county, sep = ", ") %>%
  arrange(indi_exotic_ratio) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_subcounty_bottom10_cow_ratio)

table_1_pasto_select_subcounty_bottom10_cow_ratio_plot <- table_1_pasto_select_subcounty_bottom10_cow_ratio %>%
  ggplot(aes(x = reorder(county_sub, indi_exotic_ratio), y = indi_exotic_ratio)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "goldenrod3") + 
  coord_flip() + 
  theme_classic()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "County", 
       y = "Ratio of indigenous to exotic cattle", 
       title = "Bottom 10",
       caption = "") +
  theme(axis.title.x =element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

table_1_pasto_select_subcounty_bottom10_cow_ratio_plot

ggsave("images/livestock_kenya_subcounty_pasto/subcounty_bottom10_cow_ratio.png", width = 6, height = 4)

table_1_pasto_select_subcounty_top10_cow_ratio_plot + table_1_pasto_select_subcounty_bottom10_cow_ratio_plot

ggsave("images/livestock_kenya_subcounty_pasto/subcounty_top_bottom_cow_ratio.png", width = 12, height = 4)

