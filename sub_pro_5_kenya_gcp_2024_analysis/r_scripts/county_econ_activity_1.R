# # Economic Activity per County
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

gcp_econ_activity_2023_select_tidy <- gcp_econ_activity_2023_select |>
  pivot_longer(c(agriculture_forestry_fishing:other_service_activities), 
               names_to = "econ_activity", values_to = "contribution") |>
  mutate(econ_activity = ifelse(econ_activity == "agriculture_forestry_fishing", "Agriculture",
                         ifelse(econ_activity == "mining_quarrying", "Mining",
                         ifelse(econ_activity == "manufacturing", "Manufacturing",
                         ifelse(econ_activity == "electricity_supply", "Electricity Supply",
                         ifelse(econ_activity == "water_supply_waste_collection", "Water Supply & Waste Collection",
                         ifelse(econ_activity == "construction", "Construction",
                         ifelse(econ_activity == "wholesale_retail_trade_repair_of_motor_vehicles", "Wholesale, Retail, & Motor Vehicle Repair",
                         ifelse(econ_activity == "transport_storage", "Transport & Storage",
                         ifelse(econ_activity == "accommodation_food_service_activities", "Accommodation & Food Service",
                         ifelse(econ_activity == "information_communication", "ICT",
                         ifelse(econ_activity == "financial_insurance_activities", "Financial & Insurance Services",
                         ifelse(econ_activity == "real_estate_activities", "Real Estate",
                         ifelse(econ_activity == "professional_technical_services", "Professional & Technical Services",
                         ifelse(econ_activity == "administrative_support_services", "Administrative Support Services",
                         ifelse(econ_activity == "public_administration_defence", "Public Admin & Defence",
                         ifelse(econ_activity == "education", "Education",
                         ifelse(econ_activity == "human_health_social_work_activities", "Human Health & Social Work",
                         ifelse(econ_activity == "other_service_activities", "Other Services", 
                                econ_activity))))))))))))))))))) 

# 4) Arrange by County

# Mombasa

mombasa_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Mombasa")

mombasa_gcp_econ_activity_2023_top_5 <- mombasa_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

afro_stack_palette <- c(
  "#FFB5A7", "#B5EAD7", "#9EC1CF",
  "#F6D186", "#CC79A7", "#6D8196"
)

ggplot(mombasa_gcp_econ_activity_2023_top_5, 
       aes(area = contribution, fill = group, 
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

