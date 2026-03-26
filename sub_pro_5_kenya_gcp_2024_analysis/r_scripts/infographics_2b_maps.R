# Maps for Infographics 2B 
# A single map for each of the 18 metrics
# By @kenya.in.numbers
# Data: Kenya GCP (2024)

#####################
#####PART A
#####################

# 1) Load all the required packages and libraries required for accessing the census data

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
library(scales) # control axis/scale format
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot) # plotting theme
library(gghighlight) # highlight specific data
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
library(readxl)

# 2) Load the required data

# Economic Activity by County (2023)
gcp_econ_activity_2023 <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                "datasets", "kenya_gcp_2024_tables",
                                                "gcp_econ_activity_2023.xlsx"))

# 3) Wrangle the Data

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

##############
###PART B - Arrange Data and Visualize
##############

# a) Agriculture

agriculture_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, agriculture_forestry_fishing)

agriculture_gcp_econ_activity_2023_ranked <- agriculture_gcp_econ_activity_2023 |>
  arrange(desc(agriculture_forestry_fishing)) |>
  mutate(percent_contribution = round((agriculture_forestry_fishing/sum(agriculture_forestry_fishing))*100, 1))

# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(agriculture_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
agriculture_gcp_econ_activity_2023_ranked$county <- toupper(agriculture_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(agriculture_gcp_econ_activity_2023_ranked$county)[which(!unique(agriculture_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(agriculture_gcp_econ_activity_2023_ranked$county)[which(!unique(agriculture_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_agriculture <- left_join(kenya_counties_sf, agriculture_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_agriculture <- merged_df_agriculture |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_agriculture <- ggplot(data = merged_df_agriculture)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the agricultural sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 10)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_agriculture

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_agriculture.png", width = 12, height = 12, dpi = 300)

# b) Mining

mining_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, mining_quarrying)

mining_gcp_econ_activity_2023_ranked <- mining_gcp_econ_activity_2023 |>
  arrange(desc(mining_quarrying)) |>
  mutate(percent_contribution = round((mining_quarrying/sum(mining_quarrying))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(mining_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
mining_gcp_econ_activity_2023_ranked$county <- toupper(mining_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(mining_gcp_econ_activity_2023_ranked$county)[which(!unique(mining_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(mining_gcp_econ_activity_2023_ranked$county)[which(!unique(mining_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_mining <- left_join(kenya_counties_sf, mining_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_mining <- merged_df_mining |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_mining <- ggplot(data = merged_df_mining)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the mining sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 15)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_mining

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_mining.png", width = 12, height = 12, dpi = 300)

# c) Manufacturing

manufacturing_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, manufacturing)

manufacturing_gcp_econ_activity_2023_ranked <- manufacturing_gcp_econ_activity_2023 |>
  arrange(desc(manufacturing)) |>
  mutate(percent_contribution = round((manufacturing/sum(manufacturing))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(manufacturing_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
manufacturing_gcp_econ_activity_2023_ranked$county <- toupper(manufacturing_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(manufacturing_gcp_econ_activity_2023_ranked$county)[which(!unique(manufacturing_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(manufacturing_gcp_econ_activity_2023_ranked$county)[which(!unique(manufacturing_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_manufacturing <- left_join(kenya_counties_sf, manufacturing_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_manufacturing <- merged_df_manufacturing |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_manufacturing <- ggplot(data = merged_df_manufacturing)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the manufacturing sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 40)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_manufacturing

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_manufacturing.png", width = 12, height = 12, dpi = 300)


# d) Electricity Supply

electricity_supply_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, electricity_supply)

electricity_supply_gcp_econ_activity_2023_ranked <- electricity_supply_gcp_econ_activity_2023 |>
  arrange(desc(electricity_supply)) |>
  mutate(percent_contribution = round((electricity_supply/sum(electricity_supply))*100, 1))

# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(electricity_supply_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
electricity_supply_gcp_econ_activity_2023_ranked$county <- toupper(electricity_supply_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(electricity_supply_gcp_econ_activity_2023_ranked$county)[which(!unique(electricity_supply_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(electricity_supply_gcp_econ_activity_2023_ranked$county)[which(!unique(electricity_supply_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_electricity_supply <- left_join(kenya_counties_sf, electricity_supply_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_electricity_supply <- merged_df_electricity_supply |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_electricity_supply <- ggplot(data = merged_df_electricity_supply)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the electricity supply sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 35)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_electricity_supply

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_electricity_supply.png", width = 12, height = 12, dpi = 300)


# e) Water Supply & Waste Collection

water_supply_waste_collection_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, water_supply_waste_collection)

water_supply_waste_collection_gcp_econ_activity_2023_ranked <- water_supply_waste_collection_gcp_econ_activity_2023 |>
  arrange(desc(water_supply_waste_collection)) |>
  mutate(percent_contribution = round((water_supply_waste_collection/sum(water_supply_waste_collection))*100, 1))

# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(water_supply_waste_collection_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
water_supply_waste_collection_gcp_econ_activity_2023_ranked$county <- toupper(water_supply_waste_collection_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(water_supply_waste_collection_gcp_econ_activity_2023_ranked$county)[which(!unique(water_supply_waste_collection_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(water_supply_waste_collection_gcp_econ_activity_2023_ranked$county)[which(!unique(water_supply_waste_collection_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_water_sanitation <- left_join(kenya_counties_sf, water_supply_waste_collection_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_water_sanitation <- merged_df_water_sanitation |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_water_sanitation <- ggplot(data = merged_df_water_sanitation)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to water and sanitation sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 35)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_water_sanitation

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_water_sanitation.png", width = 12, height = 12, dpi = 300)


# f) Construction

construction_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, construction)

construction_gcp_econ_activity_2023_ranked <- construction_gcp_econ_activity_2023 |>
  arrange(desc(construction)) |>
  mutate(percent_contribution = round((construction/sum(construction))*100, 1))

# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(construction_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
construction_gcp_econ_activity_2023_ranked$county <- toupper(construction_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(construction_gcp_econ_activity_2023_ranked$county)[which(!unique(construction_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(construction_gcp_econ_activity_2023_ranked$county)[which(!unique(construction_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_construction <- left_join(kenya_counties_sf, construction_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_construction <- merged_df_construction |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_construction <- ggplot(data = merged_df_construction)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the construction sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 45)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_construction

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_construction.png", width = 12, height = 12, dpi = 300)



# g) Wholesale, Retail, & Motor Vehicle Repair

wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, wholesale_retail_trade_repair_of_motor_vehicles)

wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked <- wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023 |>
  arrange(desc(wholesale_retail_trade_repair_of_motor_vehicles)) |>
  mutate(percent_contribution = round((wholesale_retail_trade_repair_of_motor_vehicles/sum(wholesale_retail_trade_repair_of_motor_vehicles))*100, 1))

# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked$county <- toupper(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked$county)[which(!unique(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked$county)[which(!unique(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_wholesale_retail_motor_vehicle_repair <- left_join(kenya_counties_sf, wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_wholesale_retail_motor_vehicle_repair <- merged_df_wholesale_retail_motor_vehicle_repair |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_wholesale_retail_motor_vehicle_repair <- ggplot(data = merged_df_wholesale_retail_motor_vehicle_repair)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the wholesale, retail, and\nmotor vehicle repair sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 50)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_wholesale_retail_motor_vehicle_repair

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_wholesale_retail_motor_vehicle_repair.png", width = 12, height = 12, dpi = 300)


# h) Transport & Storage

transport_storage_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, transport_storage)

transport_storage_gcp_econ_activity_2023_ranked <- transport_storage_gcp_econ_activity_2023 |>
  arrange(desc(transport_storage)) |>
  mutate(percent_contribution = round((transport_storage/sum(transport_storage))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(transport_storage_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
transport_storage_gcp_econ_activity_2023_ranked$county <- toupper(transport_storage_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(transport_storage_gcp_econ_activity_2023_ranked$county)[which(!unique(transport_storage_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(transport_storage_gcp_econ_activity_2023_ranked$county)[which(!unique(transport_storage_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_transport_storage <- left_join(kenya_counties_sf, transport_storage_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_transport_storage <- merged_df_transport_storage |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_transport_storage <- ggplot(data = merged_df_transport_storage)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the transport and storage sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 30)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_transport_storage

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_transport_storage.png", width = 12, height = 12, dpi = 300)


# i) Accommodation & Food Service

accommodation_food_service_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, accommodation_food_service_activities)

accommodation_food_service_activities_gcp_econ_activity_2023_ranked <- accommodation_food_service_activities_gcp_econ_activity_2023 |>
  arrange(desc(accommodation_food_service_activities)) |>
  mutate(percent_contribution = round((accommodation_food_service_activities/sum(accommodation_food_service_activities))*100, 1))

# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(accommodation_food_service_activities_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
accommodation_food_service_activities_gcp_econ_activity_2023_ranked$county <- toupper(accommodation_food_service_activities_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(accommodation_food_service_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(accommodation_food_service_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(accommodation_food_service_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(accommodation_food_service_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_accommodation_food_service <- left_join(kenya_counties_sf, accommodation_food_service_activities_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_accommodation_food_service <- merged_df_accommodation_food_service |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_accommodation_food_service <- ggplot(data = merged_df_accommodation_food_service)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the accommodation\nand food service sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 25)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_accommodation_food_service

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_accommodation_food_service.png", width = 12, height = 12, dpi = 300)




# j) ICT

information_communication_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, information_communication)

information_communication_gcp_econ_activity_2023_ranked <- information_communication_gcp_econ_activity_2023 |>
  arrange(desc(information_communication)) |>
  mutate(percent_contribution = round((information_communication/sum(information_communication))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(information_communication_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
information_communication_gcp_econ_activity_2023_ranked$county <- toupper(information_communication_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(information_communication_gcp_econ_activity_2023_ranked$county)[which(!unique(information_communication_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(information_communication_gcp_econ_activity_2023_ranked$county)[which(!unique(information_communication_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_ict <- left_join(kenya_counties_sf, information_communication_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_ict <- merged_df_ict |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_ict <- ggplot(data = merged_df_ict)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the ICT sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 50)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_ict

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_ict.png", width = 12, height = 12, dpi = 300)


# k) Financial & Insurance Services

financial_insurance_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, financial_insurance_activities)

financial_insurance_activities_gcp_econ_activity_2023_ranked <- financial_insurance_activities_gcp_econ_activity_2023 |>
  arrange(desc(financial_insurance_activities)) |>
  mutate(percent_contribution = round((financial_insurance_activities/sum(financial_insurance_activities))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(financial_insurance_activities_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
financial_insurance_activities_gcp_econ_activity_2023_ranked$county <- toupper(financial_insurance_activities_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(financial_insurance_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(financial_insurance_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(financial_insurance_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(financial_insurance_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_financial_insurance <- left_join(kenya_counties_sf, financial_insurance_activities_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_financial_insurance <- merged_df_financial_insurance |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_financial_insurance <- ggplot(data = merged_df_financial_insurance)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the financial\nand insurance services sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 75)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_financial_insurance

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_financial_insurance.png", width = 12, height = 12, dpi = 300)



# l) Real Estate

real_estate_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, real_estate_activities)

real_estate_activities_gcp_econ_activity_2023_ranked <- real_estate_activities_gcp_econ_activity_2023 |>
  arrange(desc(real_estate_activities)) |>
  mutate(percent_contribution = round((real_estate_activities/sum(real_estate_activities))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(real_estate_activities_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
real_estate_activities_gcp_econ_activity_2023_ranked$county <- toupper(real_estate_activities_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(real_estate_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(real_estate_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(real_estate_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(real_estate_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_real_estate <- left_join(kenya_counties_sf, real_estate_activities_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_real_estate <- merged_df_real_estate |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_real_estate <- ggplot(data = merged_df_real_estate)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the real estate sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 50)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_real_estate

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_real_estate.png", width = 12, height = 12, dpi = 300)



# m) Professional & Technical Services

professional_technical_services_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, professional_technical_services)

professional_technical_services_gcp_econ_activity_2023_ranked <- professional_technical_services_gcp_econ_activity_2023 |>
  arrange(desc(professional_technical_services)) |>
  mutate(percent_contribution = round((professional_technical_services/sum(professional_technical_services))*100, 1))

# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(professional_technical_services_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
professional_technical_services_gcp_econ_activity_2023_ranked$county <- toupper(professional_technical_services_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(professional_technical_services_gcp_econ_activity_2023_ranked$county)[which(!unique(professional_technical_services_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(professional_technical_services_gcp_econ_activity_2023_ranked$county)[which(!unique(professional_technical_services_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_prof_tech_services <- left_join(kenya_counties_sf, professional_technical_services_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_prof_tech_services <- merged_df_prof_tech_services |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_prof_tech_services <- ggplot(data = merged_df_prof_tech_services)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the professional\nand technical services sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 25)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_prof_tech_services

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_prof_tech_services.png", width = 12, height = 12, dpi = 300)




# n) Administrative Support Services

administrative_support_services_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, administrative_support_services)

administrative_support_services_gcp_econ_activity_2023_ranked <- administrative_support_services_gcp_econ_activity_2023 |>
  arrange(desc(administrative_support_services)) |>
  mutate(percent_contribution = round((administrative_support_services/sum(administrative_support_services))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(administrative_support_services_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
administrative_support_services_gcp_econ_activity_2023_ranked$county <- toupper(administrative_support_services_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(administrative_support_services_gcp_econ_activity_2023_ranked$county)[which(!unique(administrative_support_services_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(administrative_support_services_gcp_econ_activity_2023_ranked$county)[which(!unique(administrative_support_services_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_admin_support_services <- left_join(kenya_counties_sf, administrative_support_services_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_admin_support_services <- merged_df_admin_support_services |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_admin_support_services <- ggplot(data = merged_df_admin_support_services)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the administrative support\nservices sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 30)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_admin_support_services

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_admin_support_services.png", width = 12, height = 12, dpi = 300)


# o) Public Admin & Defence

public_administration_defence_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, public_administration_defence)

public_administration_defence_gcp_econ_activity_2023_ranked <- public_administration_defence_gcp_econ_activity_2023 |>
  arrange(desc(public_administration_defence)) |>
  mutate(percent_contribution = round((public_administration_defence/sum(public_administration_defence))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(public_administration_defence_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
public_administration_defence_gcp_econ_activity_2023_ranked$county <- toupper(public_administration_defence_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(public_administration_defence_gcp_econ_activity_2023_ranked$county)[which(!unique(public_administration_defence_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(public_administration_defence_gcp_econ_activity_2023_ranked$county)[which(!unique(public_administration_defence_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_public_admin_defence <- left_join(kenya_counties_sf, public_administration_defence_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_public_admin_defence <- merged_df_public_admin_defence |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_public_admin_defence <- ggplot(data = merged_df_public_admin_defence)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the public administration\nand defence sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 10)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_public_admin_defence

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_public_admin_defence.png", width = 12, height = 12, dpi = 300)


# p) Education

education_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, education)

education_gcp_econ_activity_2023_ranked <- education_gcp_econ_activity_2023 |>
  arrange(desc(education)) |>
  mutate(percent_contribution = round((education/sum(education))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(education_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
education_gcp_econ_activity_2023_ranked$county <- toupper(education_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(education_gcp_econ_activity_2023_ranked$county)[which(!unique(education_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(education_gcp_econ_activity_2023_ranked$county)[which(!unique(education_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_education <- left_join(kenya_counties_sf, education_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_education <- merged_df_education |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_education <- ggplot(data = merged_df_education)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the education sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 10)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_education

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_education.png", width = 12, height = 12, dpi = 300)



# q) Human Health & Social Work

human_health_social_work_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, human_health_social_work_activities)

human_health_social_work_activities_gcp_econ_activity_2023_ranked <- human_health_social_work_activities_gcp_econ_activity_2023 |>
  arrange(desc(human_health_social_work_activities)) |>
  mutate(percent_contribution = round((human_health_social_work_activities/sum(human_health_social_work_activities))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(human_health_social_work_activities_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
human_health_social_work_activities_gcp_econ_activity_2023_ranked$county <- toupper(human_health_social_work_activities_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(human_health_social_work_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(human_health_social_work_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(human_health_social_work_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(human_health_social_work_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_human_health_social_work <- left_join(kenya_counties_sf, human_health_social_work_activities_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_human_health_social_work <- merged_df_human_health_social_work |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_human_health_social_work <- ggplot(data = merged_df_human_health_social_work)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to the human health\nand social work sector (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 20)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_human_health_social_work

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_human_health_social_work.png", width = 12, height = 12, dpi = 300)


# r) Other Services

other_service_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, other_service_activities)

other_service_activities_gcp_econ_activity_2023_ranked <- other_service_activities_gcp_econ_activity_2023 |>
  arrange(desc(other_service_activities)) |>
  mutate(percent_contribution = round((other_service_activities/sum(other_service_activities))*100, 1))


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(other_service_activities_gcp_econ_activity_2023_ranked$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
other_service_activities_gcp_econ_activity_2023_ranked$county <- toupper(other_service_activities_gcp_econ_activity_2023_ranked$county)

# Inspect the county names that are different in each of the datasets
unique(other_service_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(other_service_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Fix the county names in the kenya_counties_sf dataset

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

# Inspect (AGAIN) the county names that are different in each of the datasets
unique(other_service_activities_gcp_econ_activity_2023_ranked$county)[which(!unique(other_service_activities_gcp_econ_activity_2023_ranked$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_other_services <- left_join(kenya_counties_sf, other_service_activities_gcp_econ_activity_2023_ranked, by = c("County" = "county"))

### Convert the county names to title case
merged_df_other_services <- merged_df_other_services |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Visualize the data

# Map with legend

map_other_services <- ggplot(data = merged_df_other_services)+
  geom_sf(aes(geometry = geometry, fill = percent_contribution), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Contribution to non-conventional and other services (%)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(0, 25)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_other_services

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/sector_contributions/map_other_services.png", width = 12, height = 12, dpi = 300)
