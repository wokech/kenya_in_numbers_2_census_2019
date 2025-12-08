# Arid Semi-Arid Map of Kenya

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

# Arid
arid <- c("Turkana", "Samburu", "Marsabit", "Isiolo", 
          "Mandera", "Wajir", "Garissa", "Tana River")

infographic_data_select_arid <- infographic_data_select |>
  filter(county %in% arid) |>
  mutate(color = "#000000")

infographic_data_select_arid |>
  summarise(combi = sum(total))

# Total = 4,771,106

# Semi-Arid
semi_arid <- c("Lamu", "Kwale", "Mombasa", "Kilifi", "Taita Taveta",
               "Kitui", "Makueni", "Kajiado", "Narok", "Tharaka Nithi",
               "Embu", "Meru", "Laikipia", "Baringo", "West Pokot")


infographic_data_select_semi_arid <- infographic_data_select |>
  filter(county %in% semi_arid) |>
  mutate(color = "#BB0000")

infographic_data_select_semi_arid |>
  summarise(combi = sum(total))

# Total = 12,767,138

# Generally Arable
arable <- infographic_data_select$county[!infographic_data_select$county %in% c(arid, semi_arid)]

infographic_data_select_arable <- infographic_data_select |>
  filter(county %in% arable) |>
  mutate(color = "#006600")

infographic_data_select_arable |>
  summarise(combi = sum(total))

# Total = 30,026,052

# Merge the three datasets

infographic_data_select_merged_farming <- rbind(infographic_data_select_arid,
                                                infographic_data_select_semi_arid,
                                                infographic_data_select_arable)


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
unique(infographic_data_select_merged_farming$county)[which(!unique(infographic_data_select_merged_farming$county) %in% kenya_counties_sf$COUNTY)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, infographic_data_select_merged_farming, by = c("COUNTY" = "county"))


# Draw the map

farming_map <- ggplot(data = merged_df)+
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

farming_map

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_flag_colored_maps/images/kenya_farming_1.png", width = 12, height = 12, dpi = 300)

