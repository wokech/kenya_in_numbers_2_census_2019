# # GCP Share Treemap
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
#install.packages("ggpmisc")
#library(ggpmisc) #ggplot2 extension
#webshot::install_phantomjs()
library(knitr)
library(kableExtra)
library(readxl)

# Share of the Gross County Product (5 yr avg, 2019 - 2023)
avg_share_gcp_2019_2023 <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                 "datasets", "kenya_gcp_2024_tables",
                                                 "avg_share_gcp_2019_2023.xlsx"))

avg_share_gcp_2019_2023 <- avg_share_gcp_2019_2023 %>%
  clean_names()

avg_share_gcp_2019_2023_select <- avg_share_gcp_2019_2023 %>%
  select(c(county_number, county, x5_year_avg))

# 5) Plots

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


ggsave("images/livestock_kenya_national/treemap_livestock_national.png", width = 12, height = 8, dpi = 600)

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

