# Asbestos roof use at the subcounty level
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_1 <- V4_T2.12
View(df_1)

# 4) Preliminary filtering and cleanup

# Subcounty table
table_2_sc <- df_1[4:395,] %>%
  filter(AdminArea != "County")

# Remove unnecessary columns
table_2_sc_new <- table_2_sc %>%
  select(County, SubCounty, AsbestosSheets, ConventionalHouseholds)


# Find the top 10 subcounties
top_subcounty <- table_2_sc_new %>%
  unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
  arrange(desc(AsbestosSheets)) %>%
  mutate(AffectedHouseholds = round((AsbestosSheets/100) * ConventionalHouseholds)) %>%
  slice(1:20)
View(top_subcounty)


top_subcounty_plot <- top_subcounty %>%
  ggplot(aes(x = reorder(county_sub, AsbestosSheets), y = AsbestosSheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "chocolate3") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "SubCounty", 
       y = "Percentage(%) of households with asbestos-based roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 10),
        axis.title.y =element_text(size = 10),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

top_subcounty_plot

ggsave("images/building_materials_kenya_asbestos_subcounty/top_subcounty_asbestos_plot.png", width = 6, height = 4)

# Find the bottom 10 subcounties
bottom_subcounty <- table_2_sc_new %>%
  unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
  arrange(AsbestosSheets) %>%
  mutate(AffectedHouseholds = AsbestosSheets/ConventionalHouseholds) %>%
  slice(1:20)
View(bottom_subcounty)


bottom_subcounty_plot <- bottom_subcounty %>%
  ggplot(aes(x = reorder(county_sub, AsbestosSheets), y = AsbestosSheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "chartreuse") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "SubCounty", 
       y = "Percentage (%) of households with asbestos roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 10),
        axis.title.y =element_text(size = 10),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

bottom_subcounty_plot

ggsave("images/building_materials_kenya_asbestos_subcounty/bottom_subcounty_asbestos_plot.png", width = 6, height = 4)

bottom_subcounty_plot + top_subcounty_plot 

ggsave("images/building_materials_kenya_asbestos_subcounty/top_bottom_asbestos_plot.png", width = 12, height = 6)

top_subcounty_raw <- table_2_sc_new %>%
  unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
  mutate(AffectedHouseholds = round((AsbestosSheets/100) * ConventionalHouseholds)) %>%
  arrange(desc(AffectedHouseholds)) %>%
  slice(1:10)

View(top_subcounty_raw)

# Remember that treemap is not a ggplot
#install.packages("treemapify")
library(treemapify)

ggplot(top_subcounty_raw, 
       aes(area = AffectedHouseholds, fill = AffectedHouseholds,
           label = paste(county_sub, AffectedHouseholds, sep ="\n"))) +
  geom_treemap() +
  labs(caption = "By @willyokech")+
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10,
                    grow = TRUE) + 
  theme(legend.position = "none") + 
  scale_fill_distiller(palette = "YlOrBr")

ggsave("images/building_materials_kenya_asbestos_subcounty/top_households_asbestos_treemap.png", width = 12, height = 6)

top_subcounty_raw_plot <- top_subcounty_raw %>%
  ggplot(aes(x = reorder(county_sub, AffectedHouseholds), y = AffectedHouseholds)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkorange") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "Subcounty", 
       y = "Number of households with asbestos-based roofs", 
       title = "",
       caption = "Visualization @willyokech | Source:rKenyaCensus") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

top_subcounty_raw_plot

ggsave("images/building_materials_kenya_asbestos_subcounty/top_households_asbestos_raw.png", width = 12, height = 6)
