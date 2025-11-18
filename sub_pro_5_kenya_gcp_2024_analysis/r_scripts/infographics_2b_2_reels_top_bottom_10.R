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
#install.packages("sf")
library(sf) # simple features
#install.packages("tmap") #Thematic maps 
library(tmap)
#install.packages("leaflet") # Used for creating interactive maps
library(leaflet)
#install.packages("ggbreak")
library(ggbreak)
library(patchwork)
library(ggrepel)
library(ggsflabel)

# Economic Activity by County (2023)
gcp_econ_activity_2023 <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                "datasets", "kenya_gcp_2024_tables",
                                                "gcp_econ_activity_2023.xlsx"))

# 2) Wrangle the Data

gcp_econ_activity_2023 <- gcp_econ_activity_2023 |>
  clean_names()

# a) Percentage Grouped by Economic Activity 

gcp_econ_activity_2023_sum_column <- gcp_econ_activity_2023 |>
  adorn_totals("row") |>
  filter(county_number == "Total") |>
  pivot_longer(!c(county_number, county), names_to = "activity", values_to = "share_gcp") |>
  select(activity, share_gcp) |>
  mutate(percent_share_gcp = (share_gcp*100)/13891150) |>
  filter(activity != "gcp") |>
  adorn_totals("row")

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

# 3) Visualize the data ####ADD MAP SIMILAR TO GCP SHARE####

# a) Agriculture

agriculture_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, agriculture_forestry_fishing)

agriculture_gcp_econ_activity_2023_top_bottom_10 <- agriculture_gcp_econ_activity_2023 |>
  arrange(desc(agriculture_forestry_fishing)) |>
  mutate(percent_contribution = round((agriculture_forestry_fishing/sum(agriculture_forestry_fishing))*100, 1))

# Top and Bottom 10 for Agriculture

agriculture_top_10 <- agriculture_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, agriculture_forestry_fishing, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

agriculture_bottom_10 <- agriculture_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, agriculture_forestry_fishing, percent_contribution) |>
  top_n(-10, agriculture_forestry_fishing) |>
  arrange(desc(agriculture_forestry_fishing))

# b) Mining

mining_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, mining_quarrying)

mining_gcp_econ_activity_2023_top_bottom_10 <- mining_gcp_econ_activity_2023 |>
  arrange(desc(mining_quarrying)) |>
  mutate(percent_contribution = round((mining_quarrying/sum(mining_quarrying))*100, 1))

# Top and Bottom 10 for Mining

mining_top_10 <- mining_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, mining_quarrying, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

mining_bottom_10 <- mining_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, mining_quarrying, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# c) Manufacturing

manufacturing_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, manufacturing)

manufacturing_gcp_econ_activity_2023_top_bottom_10 <- manufacturing_gcp_econ_activity_2023 |>
  arrange(desc(manufacturing)) |>
  mutate(percent_contribution = round((manufacturing/sum(manufacturing))*100, 1))

# Top and Bottom 10 for Manufacturing

manufacturing_top_10 <- manufacturing_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, manufacturing, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

manufacturing_bottom_10 <- manufacturing_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, manufacturing, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))


# d) Electricity Supply

electricity_supply_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, electricity_supply)

electricity_supply_gcp_econ_activity_2023_top_bottom_10 <- electricity_supply_gcp_econ_activity_2023 |>
  arrange(desc(electricity_supply)) |>
  mutate(percent_contribution = round((electricity_supply/sum(electricity_supply))*100, 1))

# Top and Bottom 10 for Electricity Supply

electricity_supply_top_10 <- electricity_supply_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, electricity_supply, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

electricity_supply_bottom_10 <- electricity_supply_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, electricity_supply, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# e) Water Supply & Waste Collection

water_supply_waste_collection_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, water_supply_waste_collection)

water_supply_waste_collection_gcp_econ_activity_2023_top_bottom_10 <- water_supply_waste_collection_gcp_econ_activity_2023 |>
  arrange(desc(water_supply_waste_collection)) |>
  mutate(percent_contribution = round((water_supply_waste_collection/sum(water_supply_waste_collection))*100, 1))

# Top and Bottom 10 for Water Supply & Waste Collection

water_supply_waste_collection_top_10 <- water_supply_waste_collection_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, water_supply_waste_collection, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

water_supply_waste_collection_bottom_10 <- water_supply_waste_collection_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, water_supply_waste_collection, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# f) Construction

construction_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, construction)

construction_gcp_econ_activity_2023_top_bottom_10 <- construction_gcp_econ_activity_2023 |>
  arrange(desc(construction)) |>
  mutate(percent_contribution = round((construction/sum(construction))*100, 1))

# Top and Bottom 10 for Construction

construction_top_10 <- construction_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, construction, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

construction_bottom_10 <- construction_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, construction, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# g) Wholesale, Retail, & Motor Vehicle Repair

wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, wholesale_retail_trade_repair_of_motor_vehicles)

wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_top_bottom_10 <- wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023 |>
  arrange(desc(wholesale_retail_trade_repair_of_motor_vehicles)) |>
  mutate(percent_contribution = round((wholesale_retail_trade_repair_of_motor_vehicles/sum(wholesale_retail_trade_repair_of_motor_vehicles))*100, 1))

# Top and Bottom 10 for Wholesale, Retail, & Motor Vehicle Repair

wholesale_retail_trade_repair_of_motor_vehicles_top_10 <- wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, wholesale_retail_trade_repair_of_motor_vehicles, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

wholesale_retail_trade_repair_of_motor_vehicles_bottom_10 <- wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, wholesale_retail_trade_repair_of_motor_vehicles, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# h) Transport & Storage

transport_storage_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, transport_storage)

transport_storage_gcp_econ_activity_2023_top_bottom_10 <- transport_storage_gcp_econ_activity_2023 |>
  arrange(desc(transport_storage)) |>
  mutate(percent_contribution = round((transport_storage/sum(transport_storage))*100, 1))

# Top and Bottom 10 for Transport & Storage

transport_storage_top_10 <- transport_storage_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, transport_storage, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

transport_storage_bottom_10 <- transport_storage_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, transport_storage, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# i) Accommodation & Food Service

accommodation_food_service_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, accommodation_food_service_activities)

accommodation_food_service_activities_gcp_econ_activity_2023_top_bottom_10 <- accommodation_food_service_activities_gcp_econ_activity_2023 |>
  arrange(desc(accommodation_food_service_activities)) |>
  mutate(percent_contribution = round((accommodation_food_service_activities/sum(accommodation_food_service_activities))*100, 1))

# Top and Bottom 10 for Accommodation & Food Service

accommodation_food_service_activities_top_10 <- accommodation_food_service_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, accommodation_food_service_activities, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

accommodation_food_service_activities_bottom_10 <- accommodation_food_service_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, accommodation_food_service_activities, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# j) ICT

information_communication_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, information_communication)

information_communication_gcp_econ_activity_2023_top_bottom_10 <- information_communication_gcp_econ_activity_2023 |>
  arrange(desc(information_communication)) |>
  mutate(percent_contribution = round((information_communication/sum(information_communication))*100, 1))

# Top and Bottom 10 for ICT

information_communication_top_10 <- information_communication_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, information_communication, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

information_communication_bottom_10 <- information_communication_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, information_communication, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# k) Financial & Insurance Services

financial_insurance_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, financial_insurance_activities)

financial_insurance_activities_gcp_econ_activity_2023_top_bottom_10 <- financial_insurance_activities_gcp_econ_activity_2023 |>
  arrange(desc(financial_insurance_activities)) |>
  mutate(percent_contribution = round((financial_insurance_activities/sum(financial_insurance_activities))*100, 1))

# Top and Bottom 10 for Financial & Insurance Services

financial_insurance_activities_top_10 <- financial_insurance_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, financial_insurance_activities, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

financial_insurance_activities_bottom_10 <- financial_insurance_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, financial_insurance_activities, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# l) Real Estate

real_estate_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, real_estate_activities)

real_estate_activities_gcp_econ_activity_2023_top_bottom_10 <- real_estate_activities_gcp_econ_activity_2023 |>
  arrange(desc(real_estate_activities)) |>
  mutate(percent_contribution = round((real_estate_activities/sum(real_estate_activities))*100, 1))

# Top and Bottom 10 for Real Estate

real_estate_activities_top_10 <- real_estate_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, real_estate_activities, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

real_estate_activities_bottom_10 <- real_estate_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, real_estate_activities, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# m) Professional & Technical Services

professional_technical_services_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, professional_technical_services)

professional_technical_services_gcp_econ_activity_2023_top_bottom_10 <- professional_technical_services_gcp_econ_activity_2023 |>
  arrange(desc(professional_technical_services)) |>
  mutate(percent_contribution = round((professional_technical_services/sum(professional_technical_services))*100, 1))

# Top and Bottom 10 for Professional & Technical Services

professional_technical_services_top_10 <- professional_technical_services_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, professional_technical_services, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

professional_technical_services_bottom_10 <- professional_technical_services_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, professional_technical_services, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# n) Administrative Support Services

administrative_support_services_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, administrative_support_services)

administrative_support_services_gcp_econ_activity_2023_top_bottom_10 <- administrative_support_services_gcp_econ_activity_2023 |>
  arrange(desc(administrative_support_services)) |>
  mutate(percent_contribution = round((administrative_support_services/sum(administrative_support_services))*100, 1))

# Top and Bottom 10 for Administrative Support Services

administrative_support_services_top_10 <- administrative_support_services_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, administrative_support_services, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

administrative_support_services_bottom_10 <- administrative_support_services_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, administrative_support_services, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# o) Public Admin & Defence

public_administration_defence_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, public_administration_defence)

public_administration_defence_gcp_econ_activity_2023_top_bottom_10 <- public_administration_defence_gcp_econ_activity_2023 |>
  arrange(desc(public_administration_defence)) |>
  mutate(percent_contribution = round((public_administration_defence/sum(public_administration_defence))*100, 1))

# Top and Bottom 10 for Public Admin & Defence

public_administration_defence_top_10 <- public_administration_defence_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, public_administration_defence, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

public_administration_defence_bottom_10 <- public_administration_defence_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, public_administration_defence, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# p) Education

education_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, education)

education_gcp_econ_activity_2023_top_bottom_10 <- education_gcp_econ_activity_2023 |>
  arrange(desc(education)) |>
  mutate(percent_contribution = round((education/sum(education))*100, 1))

# Top and Bottom 10 for Education

education_top_10 <- education_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, education, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

education_bottom_10 <- education_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, education, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# q) Human Health & Social Work

human_health_social_work_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, human_health_social_work_activities)

human_health_social_work_activities_gcp_econ_activity_2023_top_bottom_10 <- human_health_social_work_activities_gcp_econ_activity_2023 |>
  arrange(desc(human_health_social_work_activities)) |>
  mutate(percent_contribution = round((human_health_social_work_activities/sum(human_health_social_work_activities))*100, 1))

# Top and Bottom 10 for Human Health & Social Work

human_health_social_work_activities_top_10 <- human_health_social_work_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, human_health_social_work_activities, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

human_health_social_work_activities_bottom_10 <- human_health_social_work_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, human_health_social_work_activities, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))

# r) Other Services

other_service_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, other_service_activities)

other_service_activities_gcp_econ_activity_2023_top_bottom_10 <- other_service_activities_gcp_econ_activity_2023 |>
  arrange(desc(other_service_activities)) |>
  mutate(percent_contribution = round((other_service_activities/sum(other_service_activities))*100, 1))

# Top and Bottom 10 for Other Service Activities

other_service_activities_top_10 <- other_service_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, other_service_activities, percent_contribution) |>
  top_n(10, percent_contribution) |>
  arrange(desc(percent_contribution))

other_service_activities_bottom_10 <- other_service_activities_gcp_econ_activity_2023_top_bottom_10 |>
  select(county, other_service_activities, percent_contribution) |>
  top_n(-10, percent_contribution) |>
  arrange(desc(percent_contribution))