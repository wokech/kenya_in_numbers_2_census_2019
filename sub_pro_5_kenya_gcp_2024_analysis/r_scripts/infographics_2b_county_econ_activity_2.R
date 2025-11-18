# # Individual Economic Activity by County (2023 Data)
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

# Percentage Grouped by Economic Activity 

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

# Treemap

agriculture_gcp_econ_activity_2023_top_5 <- agriculture_gcp_econ_activity_2023 |>
  arrange(desc(agriculture_forestry_fishing)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(agriculture_forestry_fishing = sum(agriculture_forestry_fishing)) |>
  mutate(percent_contribution = round((agriculture_forestry_fishing/sum(agriculture_forestry_fishing))*100, 1))

color_map_agriculture <- c(
  "Meru" = "#FFB5A7",
  "Nakuru" = "#B5EAD7",
  "Nyandarua" = "#9EC1CF",
  "Nandi" = "#F6D186",
  "Murang'a" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
  )

ggplot(agriculture_gcp_econ_activity_2023_top_5, 
       aes(area = agriculture_forestry_fishing, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_agriculture)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/agriculture_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(agriculture_gcp_econ_activity_2023$county)[which(!unique(agriculture_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_agriculture <- left_join(kenya_counties_sf, agriculture_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_agriculture |>
  arrange(desc(agriculture_forestry_fishing)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_agriculture_with_groups_top_5 <- merged_df_agriculture |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_agriculture <- c(
  "Meru" = "#FFB5A7",
  "Nakuru" = "#B5EAD7",
  "Nyandarua" = "#9EC1CF",
  "Nandi" = "#F6D186",
  "Murang'a" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_agriculture_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_agriculture)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/agriculture_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# b) Mining

mining_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, mining_quarrying)

# Treemap

mining_gcp_econ_activity_2023_top_5 <- mining_gcp_econ_activity_2023 |>
  arrange(desc(mining_quarrying)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(mining_quarrying = sum(mining_quarrying)) |>
  mutate(percent_contribution = round((mining_quarrying/sum(mining_quarrying))*100, 1))

color_map_mining <- c(
  "Migori" = "#FFB5A7",
  "Kilifi" = "#B5EAD7",
  "Kiambu" = "#9EC1CF",
  "Kwale" = "#F6D186",
  "Machakos" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(mining_gcp_econ_activity_2023_top_5, 
       aes(area = mining_quarrying, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_mining)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/mining_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(mining_gcp_econ_activity_2023$county)[which(!unique(mining_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_mining <- left_join(kenya_counties_sf, mining_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_mining |>
  arrange(desc(mining_quarrying)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_mining_with_groups_top_5 <- merged_df_mining |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_mining <- c(
  "Migori" = "#FFB5A7",
  "Kilifi" = "#B5EAD7",
  "Kiambu" = "#9EC1CF",
  "Kwale" = "#F6D186",
  "Machakos" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_mining_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_mining)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/mining_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# c) Manufacturing

manufacturing_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, manufacturing)

# Treemap

manufacturing_gcp_econ_activity_2023_top_5 <- manufacturing_gcp_econ_activity_2023 |>
  arrange(desc(manufacturing)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(manufacturing = sum(manufacturing)) |>
  mutate(percent_contribution = round((manufacturing/sum(manufacturing))*100, 1))

color_map_manufacturing <- c(
  "Kiambu" = "#FFB5A7",
  "Kilifi" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Machakos" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(manufacturing_gcp_econ_activity_2023_top_5, 
       aes(area = manufacturing, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_manufacturing)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/manufacturing_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(manufacturing_gcp_econ_activity_2023$county)[which(!unique(manufacturing_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_manufacturing <- left_join(kenya_counties_sf, manufacturing_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_manufacturing |>
  arrange(desc(manufacturing)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_manufacturing_with_groups_top_5 <- merged_df_manufacturing |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_manufacturing <- c(
  "Kiambu" = "#FFB5A7",
  "Kilifi" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Machakos" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_manufacturing_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_manufacturing)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/manufacturing_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# d) Electricity Supply

electricity_supply_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, electricity_supply)

# Treemap

electricity_supply_gcp_econ_activity_2023_top_5 <- electricity_supply_gcp_econ_activity_2023 |>
  arrange(desc(electricity_supply)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(electricity_supply = sum(electricity_supply)) |>
  mutate(percent_contribution = round((electricity_supply/sum(electricity_supply))*100, 1))

color_map_electricity_supply <- c(
  "Embu" = "#FFB5A7",
  "Marsabit" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(electricity_supply_gcp_econ_activity_2023_top_5, 
       aes(area = electricity_supply, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_electricity_supply)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/electricity_supply_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(electricity_supply_gcp_econ_activity_2023$county)[which(!unique(electricity_supply_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_electricity_supply <- left_join(kenya_counties_sf, electricity_supply_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_electricity_supply |>
  arrange(desc(electricity_supply)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_electricity_supply_with_groups_top_5 <- merged_df_electricity_supply |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_electricity_supply <- c(
  "Embu" = "#FFB5A7",
  "Marsabit" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_electricity_supply_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_electricity_supply)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/electricity_supply_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# e) Water Supply & Waste Collection

water_supply_waste_collection_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, water_supply_waste_collection)

# Treemap

water_supply_waste_collection_gcp_econ_activity_2023_top_5 <- water_supply_waste_collection_gcp_econ_activity_2023 |>
  arrange(desc(water_supply_waste_collection)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(water_supply_waste_collection = sum(water_supply_waste_collection)) |>
  mutate(percent_contribution = round((water_supply_waste_collection/sum(water_supply_waste_collection))*100, 1))

color_map_water_supply_waste_collection <- c(
  "Kiambu" = "#FFB5A7",
  "Kilifi" = "#B5EAD7",
  "Kisumu" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(water_supply_waste_collection_gcp_econ_activity_2023_top_5, 
       aes(area = water_supply_waste_collection, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_water_supply_waste_collection)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/water_supply_waste_collection_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(water_supply_waste_collection_gcp_econ_activity_2023$county)[which(!unique(water_supply_waste_collection_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_water_supply_waste_collection <- left_join(kenya_counties_sf, water_supply_waste_collection_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_water_supply_waste_collection |>
  arrange(desc(water_supply_waste_collection)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_water_supply_waste_collection_with_groups_top_5 <- merged_df_water_supply_waste_collection |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_water_supply_waste_collection <- c(
  "Kiambu" = "#FFB5A7",
  "Kilifi" = "#B5EAD7",
  "Kisumu" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_water_supply_waste_collection_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_water_supply_waste_collection)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/water_supply_waste_collection_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# f) Construction

construction_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, construction)

# Treemap

construction_gcp_econ_activity_2023_top_5 <- construction_gcp_econ_activity_2023 |>
  arrange(desc(construction)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(construction = sum(construction)) |>
  mutate(percent_contribution = round((construction/sum(construction))*100, 1))

color_map_construction <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(construction_gcp_econ_activity_2023_top_5, 
       aes(area = construction, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_construction)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/construction_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(construction_gcp_econ_activity_2023$county)[which(!unique(construction_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_construction <- left_join(kenya_counties_sf, construction_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_construction |>
  arrange(desc(construction)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_construction_with_groups_top_5 <- merged_df_construction |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_construction <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_construction_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_construction)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/construction_top_5_counties_map.png", width = 12, height = 8, dpi = 300)


# g) Wholesale, Retail, & Motor Vehicle Repair

wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, wholesale_retail_trade_repair_of_motor_vehicles)

# Treemap

wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_top_5 <- wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023 |>
  arrange(desc(wholesale_retail_trade_repair_of_motor_vehicles)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(wholesale_retail_trade_repair_of_motor_vehicles = sum(wholesale_retail_trade_repair_of_motor_vehicles)) |>
  mutate(percent_contribution = round((wholesale_retail_trade_repair_of_motor_vehicles/sum(wholesale_retail_trade_repair_of_motor_vehicles))*100, 1))

color_map_wholesale_retail_trade_repair_of_motor_vehicles <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023_top_5, 
       aes(area = wholesale_retail_trade_repair_of_motor_vehicles, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_wholesale_retail_trade_repair_of_motor_vehicles)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/wholesale_retail_trade_repair_of_motor_vehicles_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023$county)[which(!unique(wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_wholesale_retail_trade_repair_of_motor_vehicles <- left_join(kenya_counties_sf, wholesale_retail_trade_repair_of_motor_vehicles_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_wholesale_retail_trade_repair_of_motor_vehicles |>
  arrange(desc(wholesale_retail_trade_repair_of_motor_vehicles)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_wholesale_retail_trade_repair_of_motor_vehicles_with_groups_top_5 <- merged_df_wholesale_retail_trade_repair_of_motor_vehicles |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_wholesale_retail_trade_repair_of_motor_vehicles <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_wholesale_retail_trade_repair_of_motor_vehicles_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_wholesale_retail_trade_repair_of_motor_vehicles)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/wholesale_retail_trade_repair_of_motor_vehicles_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# h) Transport & Storage

transport_storage_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, transport_storage)

# Treemap

transport_storage_gcp_econ_activity_2023_top_5 <- transport_storage_gcp_econ_activity_2023 |>
  arrange(desc(transport_storage)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(transport_storage = sum(transport_storage)) |>
  mutate(percent_contribution = round((transport_storage/sum(transport_storage))*100, 1))

color_map_transport_storage <- c(
  "Kiambu" = "#FFB5A7",
  "Kisumu" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(transport_storage_gcp_econ_activity_2023_top_5, 
       aes(area = transport_storage, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_transport_storage)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/transport_storage_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(transport_storage_gcp_econ_activity_2023$county)[which(!unique(transport_storage_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_transport_storage <- left_join(kenya_counties_sf, transport_storage_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_transport_storage |>
  arrange(desc(transport_storage)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_transport_storage_with_groups_top_5 <- merged_df_transport_storage |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_transport_storage <- c(
  "Kiambu" = "#FFB5A7",
  "Kisumu" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_transport_storage_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_transport_storage)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/transport_storage_top_5_counties_map.png", width = 12, height = 8, dpi = 300)


# i) Accommodation & Food Service

accommodation_food_service_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, accommodation_food_service_activities)

# Treemap

accommodation_food_service_activities_gcp_econ_activity_2023_top_5 <- accommodation_food_service_activities_gcp_econ_activity_2023 |>
  arrange(desc(accommodation_food_service_activities)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(accommodation_food_service_activities = sum(accommodation_food_service_activities)) |>
  mutate(percent_contribution = round((accommodation_food_service_activities/sum(accommodation_food_service_activities))*100, 1))

color_map_accommodation_food_service_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Kirinyaga" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(accommodation_food_service_activities_gcp_econ_activity_2023_top_5, 
       aes(area = accommodation_food_service_activities, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_accommodation_food_service_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/accommodation_food_service_activities_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(accommodation_food_service_activities_gcp_econ_activity_2023$county)[which(!unique(accommodation_food_service_activities_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_accommodation_food_service_activities <- left_join(kenya_counties_sf, accommodation_food_service_activities_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_accommodation_food_service_activities |>
  arrange(desc(accommodation_food_service_activities)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_accommodation_food_service_activities_with_groups_top_5 <- merged_df_accommodation_food_service_activities |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_accommodation_food_service_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Kirinyaga" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_accommodation_food_service_activities_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_accommodation_food_service_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/accommodation_food_service_activities_top_5_counties_map.png", width = 12, height = 8, dpi = 300)


# j) ICT

information_communication_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, information_communication)

# Treemap

information_communication_gcp_econ_activity_2023_top_5 <- information_communication_gcp_econ_activity_2023 |>
  arrange(desc(information_communication)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(information_communication = sum(information_communication)) |>
  mutate(percent_contribution = round((information_communication/sum(information_communication))*100, 1))

color_map_information_communication <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(information_communication_gcp_econ_activity_2023_top_5, 
       aes(area = information_communication, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_information_communication)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/information_communication_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(information_communication_gcp_econ_activity_2023$county)[which(!unique(information_communication_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_information_communication <- left_join(kenya_counties_sf, information_communication_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_information_communication |>
  arrange(desc(information_communication)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_information_communication_with_groups_top_5 <- merged_df_information_communication |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_information_communication <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_information_communication_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_information_communication)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/information_communication_top_5_counties_map.png", width = 12, height = 8, dpi = 300)


# k) Financial & Insurance Services

financial_insurance_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, financial_insurance_activities)

# Treemap

financial_insurance_activities_gcp_econ_activity_2023_top_5 <- financial_insurance_activities_gcp_econ_activity_2023 |>
  arrange(desc(financial_insurance_activities)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(financial_insurance_activities = sum(financial_insurance_activities)) |>
  mutate(percent_contribution = round((financial_insurance_activities/sum(financial_insurance_activities))*100, 1))

color_map_financial_insurance_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Uasin Gishu" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(financial_insurance_activities_gcp_econ_activity_2023_top_5, 
       aes(area = financial_insurance_activities, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_financial_insurance_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/financial_insurance_activities_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(financial_insurance_activities_gcp_econ_activity_2023$county)[which(!unique(financial_insurance_activities_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_financial_insurance_activities <- left_join(kenya_counties_sf, financial_insurance_activities_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_financial_insurance_activities |>
  arrange(desc(financial_insurance_activities)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_financial_insurance_activities_with_groups_top_5 <- merged_df_financial_insurance_activities |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_financial_insurance_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Uasin Gishu" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_financial_insurance_activities_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_financial_insurance_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/financial_insurance_activities_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# l) Real Estate

real_estate_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, real_estate_activities)

# Treemap

real_estate_activities_gcp_econ_activity_2023_top_5 <- real_estate_activities_gcp_econ_activity_2023 |>
  arrange(desc(real_estate_activities)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(real_estate_activities = sum(real_estate_activities)) |>
  mutate(percent_contribution = round((real_estate_activities/sum(real_estate_activities))*100, 1))

color_map_real_estate_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Kisumu" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Machakos" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(real_estate_activities_gcp_econ_activity_2023_top_5, 
       aes(area = real_estate_activities, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_real_estate_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/real_estate_activities_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(real_estate_activities_gcp_econ_activity_2023$county)[which(!unique(real_estate_activities_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_real_estate_activities <- left_join(kenya_counties_sf, real_estate_activities_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_real_estate_activities |>
  arrange(desc(real_estate_activities)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_real_estate_activities_with_groups_top_5 <- merged_df_real_estate_activities |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_real_estate_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Kisumu" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Machakos" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_real_estate_activities_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_real_estate_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/real_estate_activities_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# m) Professional & Technical Services

professional_technical_services_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, professional_technical_services)

# Treemap

professional_technical_services_gcp_econ_activity_2023_top_5 <- professional_technical_services_gcp_econ_activity_2023 |>
  arrange(desc(professional_technical_services)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(professional_technical_services = sum(professional_technical_services)) |>
  mutate(percent_contribution = round((professional_technical_services/sum(professional_technical_services))*100, 1))

color_map_professional_technical_services <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(professional_technical_services_gcp_econ_activity_2023_top_5, 
       aes(area = professional_technical_services, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_professional_technical_services)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/professional_technical_services_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(professional_technical_services_gcp_econ_activity_2023$county)[which(!unique(professional_technical_services_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_professional_technical_services <- left_join(kenya_counties_sf, professional_technical_services_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_professional_technical_services |>
  arrange(desc(professional_technical_services)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_professional_technical_services_with_groups_top_5 <- merged_df_professional_technical_services |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_professional_technical_services <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_professional_technical_services_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_professional_technical_services)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/professional_technical_services_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# n) Administrative Support Services

administrative_support_services_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, administrative_support_services)

# Treemap

administrative_support_services_gcp_econ_activity_2023_top_5 <- administrative_support_services_gcp_econ_activity_2023 |>
  arrange(desc(administrative_support_services)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(administrative_support_services = sum(administrative_support_services)) |>
  mutate(percent_contribution = round((administrative_support_services/sum(administrative_support_services))*100, 1))

color_map_administrative_support_services <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(administrative_support_services_gcp_econ_activity_2023_top_5, 
       aes(area = administrative_support_services, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_administrative_support_services)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/administrative_support_services_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(administrative_support_services_gcp_econ_activity_2023$county)[which(!unique(administrative_support_services_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_administrative_support_services <- left_join(kenya_counties_sf, administrative_support_services_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_administrative_support_services |>
  arrange(desc(administrative_support_services)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_administrative_support_services_with_groups_top_5 <- merged_df_administrative_support_services |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_administrative_support_services <- c(
  "Kiambu" = "#FFB5A7",
  "Machakos" = "#B5EAD7",
  "Mombasa" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_administrative_support_services_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_administrative_support_services)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/administrative_support_services_top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# o) Public Admin & Defence

public_administration_defence_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, public_administration_defence)

# Treemap

public_administration_defence_gcp_econ_activity_2023_top_5 <- public_administration_defence_gcp_econ_activity_2023 |>
  arrange(desc(public_administration_defence)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(public_administration_defence = sum(public_administration_defence)) |>
  mutate(percent_contribution = round((public_administration_defence/sum(public_administration_defence))*100, 1))

color_map_public_administration_defence <- c(
  "Kiambu" = "#FFB5A7",
  "Turkana" = "#B5EAD7",
  "Kakamega" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(public_administration_defence_gcp_econ_activity_2023_top_5, 
       aes(area = public_administration_defence, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_public_administration_defence)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/public_administration_defence_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(public_administration_defence_gcp_econ_activity_2023$county)[which(!unique(public_administration_defence_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_public_administration_defence <- left_join(kenya_counties_sf, public_administration_defence_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_public_administration_defence |>
  arrange(desc(public_administration_defence)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_public_administration_defence_with_groups_top_5 <- merged_df_public_administration_defence |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_public_administration_defence <- c(
  "Kiambu" = "#FFB5A7",
  "Turkana" = "#B5EAD7",
  "Kakamega" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_public_administration_defence_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_public_administration_defence)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/public_administration_defence_top_5_counties_map.png", width = 12, height = 8, dpi = 300)


# p) Education

education_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, education)

# Treemap

education_gcp_econ_activity_2023_top_5 <- education_gcp_econ_activity_2023 |>
  arrange(desc(education)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(education = sum(education)) |>
  mutate(percent_contribution = round((education/sum(education))*100, 1))

color_map_education <- c(
  "Kiambu" = "#FFB5A7",
  "Kakamega" = "#B5EAD7",
  "Uasin Gishu" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(education_gcp_econ_activity_2023_top_5, 
       aes(area = education, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_education)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/education_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(education_gcp_econ_activity_2023$county)[which(!unique(education_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_education <- left_join(kenya_counties_sf, education_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_education |>
  arrange(desc(education)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_education_with_groups_top_5 <- merged_df_education |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_education <- c(
  "Kiambu" = "#FFB5A7",
  "Kakamega" = "#B5EAD7",
  "Uasin Gishu" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_education_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_education)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/education_top_5_counties_map.png", width = 12, height = 8, dpi = 300)


# q) Human Health & Social Work

human_health_social_work_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, human_health_social_work_activities)

# Treemap

human_health_social_work_activities_gcp_econ_activity_2023_top_5 <- human_health_social_work_activities_gcp_econ_activity_2023 |>
  arrange(desc(human_health_social_work_activities)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(human_health_social_work_activities = sum(human_health_social_work_activities)) |>
  mutate(percent_contribution = round((human_health_social_work_activities/sum(human_health_social_work_activities))*100, 1))

color_map_human_health_social_work_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Mombasa" = "#B5EAD7",
  "Uasin Gishu" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(human_health_social_work_activities_gcp_econ_activity_2023_top_5, 
       aes(area = human_health_social_work_activities, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_human_health_social_work_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/human_health_social_work_activities_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(human_health_social_work_activities_gcp_econ_activity_2023$county)[which(!unique(human_health_social_work_activities_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_human_health_social_work_activities <- left_join(kenya_counties_sf, human_health_social_work_activities_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_human_health_social_work_activities |>
  arrange(desc(human_health_social_work_activities)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_human_health_social_work_activities_with_groups_top_5 <- merged_df_human_health_social_work_activities |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_human_health_social_work_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Mombasa" = "#B5EAD7",
  "Uasin Gishu" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)


ggplot(data = merged_df_human_health_social_work_activities_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_human_health_social_work_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/human_health_social_work_activities_top_5_counties_map.png", width = 12, height = 8, dpi = 300)


# r) Other Services

other_service_activities_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select |>
  select(county, other_service_activities)

# Treemap

other_service_activities_gcp_econ_activity_2023_top_5 <- other_service_activities_gcp_econ_activity_2023 |>
  arrange(desc(other_service_activities)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(other_service_activities = sum(other_service_activities)) |>
  mutate(percent_contribution = round((other_service_activities/sum(other_service_activities))*100, 1))

color_map_other_service_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Mombasa" = "#B5EAD7",
  "Machakos" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(other_service_activities_gcp_econ_activity_2023_top_5, 
       aes(area = other_service_activities, fill = group, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
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
  scale_fill_manual(values = color_map_other_service_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/other_service_activities_top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map Plot

# Load shapefile

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Inspect to see if names in both datasets match

unique(kenya_counties_sf$County)

# Fix names in shapefiles

kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)

### Convert to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(other_service_activities_gcp_econ_activity_2023$county)[which(!unique(other_service_activities_gcp_econ_activity_2023$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df_other_service_activities <- left_join(kenya_counties_sf, other_service_activities_gcp_econ_activity_2023, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df_other_service_activities |>
  arrange(desc(other_service_activities)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_other_service_activities_with_groups_top_5 <- merged_df_other_service_activities |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_other_service_activities <- c(
  "Kiambu" = "#FFB5A7",
  "Mombasa" = "#B5EAD7",
  "Machakos" = "#9EC1CF",
  "Nairobi City" = "#F6D186",
  "Nakuru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)



ggplot(data = merged_df_other_service_activities_with_groups_top_5)+
  geom_sf(aes(geometry = geometry, fill = group), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_other_service_activities)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_sector/other_service_activities_top_5_counties_map.png", width = 12, height = 8, dpi = 300)
