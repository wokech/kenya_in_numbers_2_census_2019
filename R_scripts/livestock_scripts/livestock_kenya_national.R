# # What types of livestock do Kenyans keep?
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
#install.packages("ggpmisc")
library(ggpmisc)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_livestock <- V4_T2.24

# Table 1 for National Analysis
table_1 <- df_livestock[1,]

table_1 <- table_1 %>%
  clean_names()

table_1_select <- table_1 %>%
  select(-c(farming, beehives, fish_ponds, fish_cages))

# 5) ggplot2 visualization

# Treemap

#install.packages("treemapify")
library(treemapify)
library(scales)

table_1_select_tidy <- table_1_select %>%
  pivot_longer(c(exotic_cattle_dairy:rabbits), 
               names_to = "livestock_type", values_to = "number") %>%
  mutate(livestock_type = ifelse(livestock_type == "exotic_cattle_dairy", "Exotic Cattle (Dairy)",
                          ifelse(livestock_type == "exotic_cattle_beef", "Exotic Cattle (Beef)",
                          ifelse(livestock_type == "indigenous_cattle", "Indigenous Cattle",
                          ifelse(livestock_type == "sheep", "Sheep",
                          ifelse(livestock_type == "goats", "Goats",
                          ifelse(livestock_type == "camels", "Camels",
                          ifelse(livestock_type == "donkeys", "Donkeys",
                          ifelse(livestock_type == "pigs", "Pigs",
                          ifelse(livestock_type == "indigenous_chicken", "Indigenous Chicken",
                          ifelse(livestock_type == "exotic_chicken_layers", "Exotic Chicken (Layers)",
                          ifelse(livestock_type == "exotic_chicken_broilers", "Exotic Chicken (Broilers)",
                          ifelse(livestock_type == "rabbits", "Rabbits", livestock_type))))))))))))) 

table_1_select_tidy

table_1_select_tidy$livestock_type

# National

# Treemap showing the livestock populations

# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy, 
       aes(area = number, fill = livestock_type, 
           label = livestock_type)) +
  geom_treemap() +
  labs(title = "Livestock in Kenya",
       subtitle = "The relative numbers of livestock owned by farming households in Kenya (2019)",
       fill = "",
       caption = "Data Source: rKenyaCensus | By @willyokech") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10,
                    grow = TRUE) + 
  theme(legend.position = "bottom",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_brewer(palette = "Paired") 


ggsave("images/livestock_kenya_national/treemap_livestock_national.png", width = 12, height = 8)

table_1_select_tidy_print <- data.frame(livestock_type = table_1_select_tidy$livestock_type, 
                                        number = table_1_select_tidy$number)

table_1_select_tidy_print %>%
  arrange(desc(number)) %>%
  rename("Livestock Type" = "livestock_type",
         "Number" = "number") %>%
  kbl(align = "c", format.args = list(big.mark = ",")) %>%
  kable_classic() %>% 
  row_spec(row = 0, font_size = 28, color = "white", background = "#000000") %>%
  row_spec(row = c(1:12), font_size = 20) %>%
  save_kable(file = "images/livestock_kenya_national/table_livestock_national.png",
             zoom = 5)

