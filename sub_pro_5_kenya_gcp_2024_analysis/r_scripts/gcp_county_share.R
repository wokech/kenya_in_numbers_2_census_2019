# # GCP Share Treemap
# By @willyokech
# Data: Kenya GCP 2024

# 1) Load the required packages

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
#install.packages("treemapify")
library(treemapify)
library(scales)

# 2) Load the data

# Share of the Gross County Product (5 yr avg, 2019 - 2023)
avg_share_gcp_2019_2023 <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                 "datasets", "kenya_gcp_2024_tables",
                                                 "avg_share_gcp_2019_2023.xlsx"))

# 3) Wrangle the Data

avg_share_gcp_2019_2023 <- avg_share_gcp_2019_2023 |>
  clean_names()

avg_share_gcp_2019_2023_select <- avg_share_gcp_2019_2023 |>
  select(c(county_number, county, x5_year_avg))

unique(avg_share_gcp_2019_2023_select$county)
avg_share_gcp_2019_2023_select$county <- gsub("/", " ", avg_share_gcp_2019_2023_select$county)
avg_share_gcp_2019_2023_select$county <- gsub("-", " ", avg_share_gcp_2019_2023_select$county)
avg_share_gcp_2019_2023_select <- avg_share_gcp_2019_2023_select |> 
  mutate(county = tools::toTitleCase(tolower(county)))
unique(avg_share_gcp_2019_2023_select$county)

# Rename Murang'a
avg_share_gcp_2019_2023_select <- avg_share_gcp_2019_2023_select |>
  mutate(county = recode(county, "Murang’a" = "Murang'a"))

# 4) Arrange by top 5 counties and Visualize

avg_share_gcp_2019_2023_select_tidy <- avg_share_gcp_2019_2023_select |>
  select(county, x5_year_avg) |>
  filter(county != "Total")

avg_share_gcp_2019_2023_select_tidy_top_5 <- avg_share_gcp_2019_2023_select_tidy |>
  arrange(desc(x5_year_avg)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(x5_year_avg = sum(x5_year_avg))

# Visualize the data

afro_stack_palette <- c(
  "#FFB5A7", "#B5EAD7", "#9EC1CF",
  "#F6D186", "#CC79A7", "#6D8196"
)

ggplot(avg_share_gcp_2019_2023_select_tidy_top_5, 
       aes(area = x5_year_avg, fill = group, 
           label = paste0(group, "\n",
                          x5_year_avg, "%"))) +
  geom_treemap() +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_manual(values = afro_stack_palette)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/top_5_counties.png", width = 12, height = 12, dpi = 300)

# 5) Arrange by cities and non-cities

# Define the city counties of interest
city_counties <- c("Nairobi City", "Mombasa", "Kisumu", "Nakuru", "Uasin Gishu")

# Group all others as "Others"
avg_share_gcp_2019_2023_select_tidy_city <- avg_share_gcp_2019_2023_select_tidy %>%
  mutate(group = if_else(county %in% city_counties, county, "Others")) %>%
  group_by(group) %>%
  summarise(x5_year_avg = sum(x5_year_avg))

# Visualize the data

afro_stack_palette <- c(
  "#FFB5A7", "#B5EAD7", "#9EC1CF",
  "#F6D186", "#CC79A7", "#6D8196"
)

ggplot(avg_share_gcp_2019_2023_select_tidy_city, 
       aes(area = x5_year_avg, fill = group, 
           label = paste0(group, "\n",
                          x5_year_avg, "%"))) +
  geom_treemap() +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_manual(values = afro_stack_palette)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/city_counties.png", width = 12, height = 12, dpi = 300)



# 5) Arrange by Nairobi Metro and Non-Metro

# Define the counties of interest
metro_counties <- c("Nairobi City", "Kiambu", "Machakos", "Kajiado", "Murang'a")

# Group all others as "Others"
avg_share_gcp_2019_2023_select_tidy_metro <- avg_share_gcp_2019_2023_select_tidy %>%
  mutate(group = if_else(county %in% metro_counties, county, "Others")) %>%
  group_by(group) %>%
  summarise(x5_year_avg = sum(x5_year_avg))

# Visualize the data

afro_stack_palette <- c(
  "#FFB5A7", "#B5EAD7", "#9EC1CF",
  "#F6D186", "#CC79A7", "#6D8196"
)

ggplot(avg_share_gcp_2019_2023_select_tidy_metro, 
       aes(area = x5_year_avg, fill = group, 
           label = paste0(group, "\n",
                          x5_year_avg, "%"))) +
  geom_treemap() +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_manual(values = afro_stack_palette)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/metro_counties.png", width = 12, height = 12, dpi = 300)

