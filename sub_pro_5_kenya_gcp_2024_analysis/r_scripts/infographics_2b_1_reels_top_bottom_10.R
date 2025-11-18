# Reels for the Infographic 2b Datasets
# By @kenya.in.numbers

# Data: Data: Census(2019)

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
library(readxl)

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

# Rename Murang'a and add totals
gcp_econ_activity_2023_select <- gcp_econ_activity_2023_select |>
  mutate(county = recode(county, "Murang’a" = "Murang'a")) |>
  adorn_totals("row")

# Rename county to "Total"
gcp_econ_activity_2023_select[48, 2] <- "Total"

# 3a) Visualize the data

gcp_econ_activity_2023_select_tidy <- gcp_econ_activity_2023_select |>
  pivot_longer(c(agriculture_forestry_fishing:other_service_activities), 
               names_to = "econ_activity", values_to = "contribution") |>
  mutate(econ_activity = ifelse(econ_activity == "agriculture_forestry_fishing", "Agriculture",
                                ifelse(econ_activity == "mining_quarrying", "Mining",
                                       ifelse(econ_activity == "manufacturing", "Manufacturing",
                                              ifelse(econ_activity == "electricity_supply", "Electricity Supply",
                                                     ifelse(econ_activity == "water_supply_waste_collection", "Water Supply & Waste Collection",
                                                            ifelse(econ_activity == "construction", "Construction",
                                                                   ifelse(econ_activity == "wholesale_retail_trade_repair_of_motor_vehicles", "Wholesale, Retail, &\nMotor Vehicle Repair",
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

# 3b) Country-Level Data

# Kenya

kenya_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Total")

kenya_gcp_econ_activity_2023_top_bottom_5 <- kenya_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))


# 4) Top and Bottom 5 for Econ Activity

# Fix the county names

kenya_gcp_econ_activity_2023_top_5 <- kenya_gcp_econ_activity_2023_top_bottom_5 |>
  select(econ_activity, percent_contribution) |>
  top_n(5, percent_contribution) |>
  arrange(desc(percent_contribution))

kenya_gcp_econ_activity_2023_bottom_5 <- kenya_gcp_econ_activity_2023_top_bottom_5 |>
  select(econ_activity, percent_contribution) |>
  top_n(-5, percent_contribution) |>
  arrange(desc(percent_contribution))

