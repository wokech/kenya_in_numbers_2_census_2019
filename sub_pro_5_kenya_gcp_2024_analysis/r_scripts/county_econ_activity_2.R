# # Individual Economic Activity by County
# By @kenya.in.numbers
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
#install.packages("treemapify")
library(treemapify)
library(scales)

# Economic Activity by County (2023)
gcp_econ_activity_2023 <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                "datasets", "kenya_gcp_2024_tables",
                                                "gcp_econ_activity_2023.xlsx"))

# 2) Wrangle the Data

gcp_econ_activity_2023 <- gcp_econ_activity_2023 |>
  clean_names()

gcp_econ_activity_2023_select <- gcp_econ_activity_2023 |>
  select(-c(financial_services_indirectly_measured, gcp))

unique(gcp_econ_activity_2023_select$county)
gcp_econ_activity_2023_select$county <- gsub("/", " ", gcp_econ_activity_2023_select$county)
gcp_econ_activity_2023_select$county <- gsub("-", " ", gcp_econ_activity_2023_select$county)
gcp_econ_activity_2023_select <- gcp_econ_activity_2023_select |> 
  mutate(county = tools::toTitleCase(tolower(county)))
unique(gcp_econ_activity_2023_select$county)

# Rename Murang'a
gcp_econ_activity_2023_select <- gcp_econ_activity_2023_select |>
  mutate(county = recode(county, "Murang’a" = "Murang'a"))

# 3) Visualize the data

agriculture_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, agriculture_forestry_fishing)

agriculture_gcp_econ_activity_2023_top_5 <- agriculture_gcp_econ_activity_2023 |>
  arrange(desc(agriculture_forestry_fishing)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(agriculture_forestry_fishing = sum(agriculture_forestry_fishing)) |>
  mutate(percent_contribution = round((agriculture_forestry_fishing/sum(agriculture_forestry_fishing))*100, 1))

# Visualize the data

afro_stack_palette <- c(
  "#FFB5A7", "#B5EAD7", "#9EC1CF",
  "#F6D186", "#CC79A7", "#6D8196"
)

ggplot(agriculture_gcp_econ_activity_2023_top_5, 
       aes(area = agriculture_forestry_fishing, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
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

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/top_5_counties.png", width = 12, height = 12, dpi = 300)

