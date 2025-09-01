# Map of Kenya divided and colored into 3
# William Okech

# Load libraries

library(tidyverse)
library(readxl)
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
library(ggtext)
library(rKenyaCensus)

# Load data
infographic_data <- read_csv(here::here("sub_pro_4_kenya_infographics", 
                                        "datasets", "infographic_data_1.csv"))

# Select the population and county columns
infographic_data_select <- infographic_data |>
  select(County, Total) |>
  clean_names() |>
  filter(county != "Kenya")

# Total Population = 47,564,296
# Approximate third = 15,854,765

# Lower Third
infographic_data_select_lower_third <- infographic_data_select |>
  filter(county %in% c("Kwale", "Lamu", "Mombasa",
                       "Kilifi", "Tana River", "Garissa",
                       "Kitui", "Taita Taveta", "Makueni",
                       "Machakos", "Kajiado", "Nairobi City",
                       "Embu", "Tharaka Nithi", "Kirinyaga")) |>
  mutate(color = "#006600")

infographic_data_select_lower_third |>
  summarise(combi = sum(total))

# Total = 15,843,699

# Middle Third
infographic_data_select_middle_third <- infographic_data_select |>
  filter(county %in% c("Narok", "Nakuru", "Kiambu",
                       "Nyandarua", "Murang'a", "Nyeri",
                       "Laikipia", "Isiolo", "Wajir",
                       "Mandera", "Bomet", "Migori",
                       "Kisii", "Nyamira", "Meru")) |>
  mutate(color = "#BB0000")

infographic_data_select_middle_third |>
  summarise(combi = sum(total))

# Total = 16,037,460

# Top Third
infographic_data_select_top_third <- infographic_data_select |>
  filter(county %in% c("Marsabit", "Samburu", "Turkana",
                       "West Pokot", "Busia", "Bungoma",
                       "Trans Nzoia", "Siaya", "Kakamega",
                       "Kisumu", "Nandi", "Uasin Gishu",
                       "Homa Bay", "Elgeyo Marakwet", "Baringo",
                       "Vihiga", "Kericho")) |>
  mutate(color = "#000000")

infographic_data_select_middle_third |>
  summarise(combi = sum(total))

# Total = 15,683,137

# Merge the three datasets

infographic_data_select_merged_third <- rbind(infographic_data_select_top_third,
                                              infographic_data_select_middle_third,
                                              infographic_data_select_lower_third)


# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_read(here::here("sub_pro_1_kenya_county_sub_county", 
                                        "kenyan-counties-shapefile", "County.shp"))

# Plot a void map of Kenya
ggplot(kenya_counties_sf) + 
  geom_sf() + 
  theme_void()

# 6) Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$COUNTY)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Rename counties in the sf table
kenya_counties_sf <- kenya_counties_sf |>
  mutate(COUNTY = recode(COUNTY, "Keiyo-Marakwet" = "Elgeyo Marakwet"),
         COUNTY = recode(COUNTY, "Tharaka" = "Tharaka Nithi"),
         COUNTY = recode(COUNTY, "Nairobi" = "Nairobi City"))

# Inspect the county names that are different in each of the datasets
unique(infographic_data_select_merged_third$county)[which(!unique(infographic_data_select_merged_third$county) %in% kenya_counties_sf$COUNTY)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, infographic_data_select_merged_third, by = c("COUNTY" = "county"))


# Draw the map

flag_map <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = color), linewidth = 0.35, color = "white")+
  scale_fill_identity() +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

flag_map
