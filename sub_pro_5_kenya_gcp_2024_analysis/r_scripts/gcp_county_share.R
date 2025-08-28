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

# Top 10 Share GCP

share_GCP_top_10 <- avg_share_gcp_2019_2023_select |>
  select(county, x5_year_avg) |>
  filter(county != "Total") |>
  top_n(10) |>
  arrange(desc(x5_year_avg))

# Bottom 10 Share GCP

share_GCP_bottom_10 <- avg_share_gcp_2019_2023_select |>
  select(county, x5_year_avg) |>
  filter(county != "Total") |>
  top_n(-10) |>
  arrange(x5_year_avg)

 # 4) Visualize

# a) Top 5 Counties

avg_share_gcp_2019_2023_select_tidy <- avg_share_gcp_2019_2023_select |>
  select(county, x5_year_avg) |>
  filter(county != "Total")

avg_share_gcp_2019_2023_select_tidy_top_5 <- avg_share_gcp_2019_2023_select_tidy |>
  arrange(desc(x5_year_avg)) |>
  mutate(group = if_else(row_number() <= 5,
                         county, "Other Counties")) |>
  group_by(group) |>
  summarise(x5_year_avg = sum(x5_year_avg))

# Treemap

color_map_top_5 <- c(
  "Nairobi City" = "#FFB5A7",
  "Kiambu" = "#B5EAD7",
  "Nakuru" = "#9EC1CF",
  "Mombasa" = "#F6D186",
  "Meru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(avg_share_gcp_2019_2023_select_tidy_top_5, 
       aes(area = x5_year_avg, fill = group, 
           label = paste0(group, "\n",
                          x5_year_avg, "%"))) +
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
  scale_fill_manual(values = color_map_top_5)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/top_5_counties_treemap.png", width = 12, height = 8, dpi = 300)

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
unique(avg_share_gcp_2019_2023_select_tidy$county)[which(!unique(avg_share_gcp_2019_2023_select_tidy$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, avg_share_gcp_2019_2023_select_tidy, by = c("County" = "county"))

# Identify top 5 counties
top5 <- merged_df |>
  arrange(desc(x5_year_avg)) |>
  slice_head(n = 5) |>
  pull(County)

# Add group column to full tidy dataset
merged_df_with_groups_top_5 <- merged_df |>
  mutate(group = if_else(County %in% top5, County, "Other Counties"))

color_map_top_5 <- c(
  "Nairobi City" = "#FFB5A7",
  "Kiambu" = "#B5EAD7",
  "Nakuru" = "#9EC1CF",
  "Mombasa" = "#F6D186",
  "Meru" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_with_groups_top_5)+
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
  scale_fill_manual(values = color_map_top_5)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/top_5_counties_map.png", width = 12, height = 8, dpi = 300)

# b) Arrange by cities and non-cities

# Define the city counties of interest
city_counties <- c("Nairobi City", "Mombasa", "Kisumu", "Nakuru", "Uasin Gishu")

avg_share_gcp_2019_2023_select_tidy <- avg_share_gcp_2019_2023_select |>
  select(county, x5_year_avg) |>
  filter(county != "Total")

# Group all others as "Others"
avg_share_gcp_2019_2023_select_tidy_city <- avg_share_gcp_2019_2023_select_tidy %>%
  mutate(group = if_else(county %in% city_counties, county, "Other Counties")) %>%
  group_by(group) %>%
  summarise(x5_year_avg = sum(x5_year_avg))

# Treemap

color_map_city <- c(
  "Nairobi City" = "#FFB5A7",
  "Mombasa" = "#B5EAD7",
  "Kisumu" = "#9EC1CF",
  "Nakuru" = "#F6D186",
  "Uasin Gishu" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(avg_share_gcp_2019_2023_select_tidy_city, 
       aes(area = x5_year_avg, fill = group, 
           label = paste0(group, "\n",
                          x5_year_avg, "%"))) +
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
  scale_fill_manual(values = color_map_city)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/city_counties_treemap.png", width = 12, height = 8, dpi = 300)

# Map

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
unique(avg_share_gcp_2019_2023_select_tidy$county)[which(!unique(avg_share_gcp_2019_2023_select_tidy$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, avg_share_gcp_2019_2023_select_tidy, by = c("County" = "county"))

# Add group column to full tidy dataset
merged_df_with_groups_city_county <- merged_df |>
  mutate(group = if_else(County %in% city_counties, County, "Other Counties"))

color_map_city <- c(
  "Nairobi City" = "#FFB5A7",
  "Mombasa" = "#B5EAD7",
  "Kisumu" = "#9EC1CF",
  "Nakuru" = "#F6D186",
  "Uasin Gishu" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_with_groups_city_county)+
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
  scale_fill_manual(values = color_map_city)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/city_counties_map.png", width = 12, height = 8, dpi = 300)


# c) Arrange by Nairobi Metro and Non-Metro

# Define the counties of interest
metro_counties <- c("Nairobi City", "Kiambu", "Machakos", "Kajiado", "Murang'a")

# Group all others as "Others"
avg_share_gcp_2019_2023_select_tidy_metro <- avg_share_gcp_2019_2023_select_tidy %>%
  mutate(group = if_else(county %in% metro_counties, county, "Other Counties")) %>%
  group_by(group) %>%
  summarise(x5_year_avg = sum(x5_year_avg))

# Treemap

color_map_metro <- c(
  "Nairobi City" = "#FFB5A7",
  "Kiambu" = "#B5EAD7",
  "Machakos" = "#9EC1CF",
  "Kajiado" = "#F6D186",
  "Murang'a" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(avg_share_gcp_2019_2023_select_tidy_metro, 
       aes(area = x5_year_avg, fill = group, 
           label = paste0(group, "\n",
                          x5_year_avg, "%"))) +
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
  scale_fill_manual(values = color_map_metro)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/metro_counties.png", width = 12, height = 8, dpi = 300)

# Map

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
unique(avg_share_gcp_2019_2023_select_tidy$county)[which(!unique(avg_share_gcp_2019_2023_select_tidy$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, avg_share_gcp_2019_2023_select_tidy, by = c("County" = "county"))

# Add group column to full tidy dataset
merged_df_with_groups_top_5 <- merged_df |>
  mutate(group = if_else(County %in% metro_counties, County, "Other Counties"))

color_map_metro <- c(
  "Nairobi City" = "#FFB5A7",
  "Kiambu" = "#B5EAD7",
  "Machakos" = "#9EC1CF",
  "Kajiado" = "#F6D186",
  "Murang'a" = "#CC79A7",
  "Other Counties" = "#BEBEBE"  # For grouped others
)

ggplot(data = merged_df_with_groups_top_5)+
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
  scale_fill_manual(values = color_map_metro)

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/metro_counties_map.png", width = 12, height = 8, dpi = 300)


# 9) GCP Share (%)

map_gcp_share <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = x5_year_avg), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Share of the Gross County Product (GCP, %)")+
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
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_gcp_share

# Save the plot

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_county_share/infographics_1_maps_top_bottom_gcp_share.png", width = 12, height = 12, dpi = 300)

