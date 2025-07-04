# County Maps and Headquarter Locations
# By @kenya.in.numbers
# Intro maps for the counties
# From the Kenya Population and Housing Census Report (2019) and rKenyaCensus

#####################
#####PART A
#####################

# 1) Load all the required packages and libraries required for accessing the census data

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
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

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Plot a void map of Kenya
full_map_kenya <- ggplot(kenya_counties_sf) + 
  geom_sf(fill = 'goldenrod3', linewidth = 0.1, color = "black") + 
  theme_void() +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

full_map_kenya

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/full_map_kenya.png", width = 3, height = 3, dpi = 300)

# Void map for Canva

full_map_kenya_canva <- ggplot(kenya_counties_sf) + 
  geom_sf(fill = 'goldenrod3', linewidth = 0.05, color = 'goldenrod3') + 
  theme_void()

full_map_kenya_canva

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/full_map_kenya_canva.png", width = 3, height = 3, dpi = 300)

# Change the title case of the counties and fix the names

unique(kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Headquarters data
# Read in the data
headquarters <- read_excel("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/datasets/kenyan_county_headquarters.xlsx")
# Split the latitude and longitude data
headquarters_split <- headquarters |>
  separate("Latitude, Longitude", into = c("latitude", "longitude"), sep = ",")  |>
  mutate(across(everything(), ~trimws(.)))
# transform to sf data
headquarters_sf <- st_as_sf(headquarters_split, coords = c("longitude", "latitude"), crs = 4326)
# transform to match the shapefile CRS
headquarters_sf <- st_transform(headquarters_sf, st_crs(kenya_counties_sf))

#####################
#####PART B - Visualize Data
#####################

# Void map with highlighted Capital City for Canva

highlighted <- subset(kenya_counties_sf, County == "Nairobi City")
others <- subset(kenya_counties_sf, County != "Nairobi City")

ggplot() +
  geom_sf(data = others, fill = "goldenrod2", color = "goldenrod2", linewidth = 0.01) +
  geom_sf(data = highlighted, fill = "navy", color = "black", linewidth = 0.0) +
  theme_void()

ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/full_map_kenya_capital_canva.png", width = 3, height = 3, dpi = 300)

# County 001 - Mombasa

mombasa <- c("Mombasa")
mombasa_df <- kenya_counties_sf |> filter(County %in% mombasa)
headquarters_mombasa_sf <- headquarters_sf |> filter(County %in% mombasa)

map_mombasa <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% mombasa) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mombasa

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_001_mombasa_canva.png", width = 3, height = 3, dpi = 300)

map_mombasa_zoom <- ggplot(data = mombasa_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_mombasa_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mombasa_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_001_mombasa_canva.png", width = 3, height = 3, dpi = 300)

# County 002 - Kwale

kwale <- c("Kwale")
kwale_df <- kenya_counties_sf |> filter(County %in% kwale)
headquarters_kwale_sf <- headquarters_sf |> filter(County %in% kwale)

map_kwale <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kwale) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kwale

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_002_kwale_canva.png", width = 3, height = 3, dpi = 300)

map_kwale_zoom <- ggplot(data = kwale_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kwale_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kwale_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_002_kwale_canva.png", width = 3, height = 3, dpi = 300)

# County 003 - Kilifi

kilifi <- c("Kilifi")
kilifi_df <- kenya_counties_sf |> filter(County %in% kilifi)
headquarters_kilifi_sf <- headquarters_sf |> filter(County %in% kilifi)


map_kilifi <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kilifi) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kilifi

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_003_kilifi_canva.png", width = 3, height = 3, dpi = 300)

map_kilifi_zoom <- ggplot(data = kilifi_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kilifi_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kilifi_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_003_kilifi_canva.png", width = 3, height = 3, dpi = 300)

# County 004 - Tana River

tana_river <- c("Tana River")
tana_river_df <- kenya_counties_sf |> filter(County %in% tana_river)
headquarters_tana_river_sf <- headquarters_sf |> filter(County %in% tana_river)

map_tana_river <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% tana_river) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_tana_river

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_004_tana_river_canva.png", width = 3, height = 3, dpi = 300)

map_tana_river_zoom <- ggplot(data = tana_river_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_tana_river_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_tana_river_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_004_tana_river_canva.png", width = 3, height = 3, dpi = 300)

# County 005 - Lamu

lamu <- c("Lamu")
lamu_df <- kenya_counties_sf |> filter(County %in% lamu)
headquarters_lamu_sf <- headquarters_sf |> filter(County %in% lamu)

map_lamu <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% lamu) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_lamu

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_005_lamu_canva.png", width = 3, height = 3, dpi = 300)

map_lamu_zoom <- ggplot(data = lamu_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_lamu_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_lamu_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_005_lamu_canva.png", width = 3, height = 3, dpi = 300)

# County 006 - Taita Taveta 

taita_taveta <- c("Taita Taveta")
taita_taveta_df <- kenya_counties_sf |> filter(County %in% taita_taveta)
headquarters_taita_taveta_sf <- headquarters_sf |> filter(County %in% taita_taveta)

map_taita_taveta <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% taita_taveta) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_taita_taveta

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_006_taita_taveta_canva.png", width = 3, height = 3, dpi = 300)

map_taita_taveta_zoom <- ggplot(data = taita_taveta_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_taita_taveta_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_taita_taveta_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_006_taita_taveta_canva.png", width = 3, height = 3, dpi = 300)

# County 007 - Garissa 

garissa <- c("Garissa")
garissa_df <- kenya_counties_sf |> filter(County %in% garissa)
headquarters_garissa_sf <- headquarters_sf |> filter(County %in% garissa)

map_garissa <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% garissa) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_garissa

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_007_garissa_canva.png", width = 3, height = 3, dpi = 300)

map_garissa_zoom <- ggplot(data = garissa_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_garissa_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_garissa_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_007_garissa_canva.png", width = 3, height = 3, dpi = 300)

# County 008 - Wajir

wajir <- c("Wajir")
wajir_df <- kenya_counties_sf |> filter(County %in% wajir)
headquarters_wajir_sf <- headquarters_sf |> filter(County %in% wajir)

map_wajir <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% wajir) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_wajir

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_008_wajir_canva.png", width = 3, height = 3, dpi = 300)

map_wajir_zoom <- ggplot(data = wajir_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_wajir_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_wajir_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_008_wajir_canva.png", width = 3, height = 3, dpi = 300)

# County 009 - Mandera

mandera <- c("Mandera")
mandera_df <- kenya_counties_sf |> filter(County %in% mandera)
headquarters_mandera_sf <- headquarters_sf |> filter(County %in% mandera)

map_mandera <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% mandera) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mandera

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_009_mandera_canva.png", width = 3, height = 3, dpi = 300)

map_mandera_zoom <- ggplot(data = mandera_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_mandera_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mandera_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_009_mandera_canva.png", width = 3, height = 3, dpi = 300)

# County 010 - Marsabit

marsabit <- c("Marsabit")
marsabit_df <- kenya_counties_sf |> filter(County %in% marsabit)
headquarters_marsabit_sf <- headquarters_sf |> filter(County %in% marsabit)

map_marsabit <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% marsabit) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_marsabit

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_010_marsabit_canva.png", width = 3, height = 3, dpi = 300)

map_marsabit_zoom <- ggplot(data = marsabit_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_marsabit_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_marsabit_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_010_marsabit_canva.png", width = 3, height = 3, dpi = 300)

# County 011 - Isiolo

isiolo <- c("Isiolo")
isiolo_df <- kenya_counties_sf |> filter(County %in% isiolo)
headquarters_isiolo_sf <- headquarters_sf |> filter(County %in% isiolo)

map_isiolo <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% isiolo) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_isiolo

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_011_isiolo_canva.png", width = 3, height = 3, dpi = 300)

map_isiolo_zoom <- ggplot(data = isiolo_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_isiolo_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_isiolo_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_011_isiolo_canva.png", width = 3, height = 3, dpi = 300)

# County 012 - Meru

meru <- c("Meru")
meru_df <- kenya_counties_sf |> filter(County %in% meru)
headquarters_meru_sf <- headquarters_sf |> filter(County %in% meru)

map_meru <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% meru) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_meru

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_012_meru_canva.png", width = 3, height = 3, dpi = 300)

map_meru_zoom <- ggplot(data = meru_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_meru_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_meru_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_012_meru_canva.png", width = 3, height = 3, dpi = 300)

# County 013 - Tharaka Nithi

tharaka_nithi <- c("Tharaka Nithi")
tharaka_nithi_df <- kenya_counties_sf |> filter(County %in% tharaka_nithi)
headquarters_tharaka_nithi_sf <- headquarters_sf |> filter(County %in% tharaka_nithi)

map_tharaka_nithi <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% tharaka_nithi) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_tharaka_nithi

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_013_tharaka_nithi_canva.png", width = 3, height = 3, dpi = 300)

map_tharaka_nithi_zoom <- ggplot(data = tharaka_nithi_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_tharaka_nithi_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_tharaka_nithi_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_013_tharaka_nithi_canva.png", width = 3, height = 3, dpi = 300)

# County 014 - Embu

embu <- c("Embu")
embu_df <- kenya_counties_sf |> filter(County %in% embu)
headquarters_embu_sf <- headquarters_sf |> filter(County %in% embu)

map_embu <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% embu) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_embu

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_014_embu_canva.png", width = 3, height = 3, dpi = 300)

map_embu_zoom <- ggplot(data = embu_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_embu_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_embu_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_014_embu_canva.png", width = 3, height = 3, dpi = 300)

# County 015 - Kitui

kitui <- c("Kitui")
kitui_df <- kenya_counties_sf |> filter(County %in% kitui)
headquarters_kitui_sf <- headquarters_sf |> filter(County %in% kitui)

map_kitui <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kitui) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kitui

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_015_kitui_canva.png", width = 3, height = 3, dpi = 300)

map_kitui_zoom <- ggplot(data = kitui_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kitui_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kitui_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_015_kitui_canva.png", width = 3, height = 3, dpi = 300)

# County 016 - Machakos

machakos <- c("Machakos")
machakos_df <- kenya_counties_sf |> filter(County %in% machakos)
headquarters_machakos_sf <- headquarters_sf |> filter(County %in% machakos)

map_machakos <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% machakos) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_machakos

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_016_machakos_canva.png", width = 3, height = 3, dpi = 300)

map_machakos_zoom <- ggplot(data = machakos_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_machakos_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_machakos_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_016_machakos_canva.png", width = 3, height = 3, dpi = 300)

# County 017 - Makueni

makueni <- c("Makueni")
makueni_df <- kenya_counties_sf |> filter(County %in% makueni)
headquarters_makueni_sf <- headquarters_sf |> filter(County %in% makueni)

map_makueni <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% makueni) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_makueni

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_017_makueni_canva.png", width = 3, height = 3, dpi = 300)

map_makueni_zoom <- ggplot(data = makueni_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_makueni_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_makueni_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_017_makueni_canva.png", width = 3, height = 3, dpi = 300)

# County 018 - Nyandarua

nyandarua <- c("Nyandarua")
nyandarua_df <- kenya_counties_sf |> filter(County %in% nyandarua)
headquarters_nyandarua_sf <- headquarters_sf |> filter(County %in% nyandarua)

map_nyandarua <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% nyandarua) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nyandarua

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_018_nyandarua_canva.png", width = 3, height = 3, dpi = 300)

map_nyandarua_zoom <- ggplot(data = nyandarua_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_nyandarua_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nyandarua_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_018_nyandarua_canva.png", width = 3, height = 3, dpi = 300)

# County 019 - Nyeri

nyeri <- c("Nyeri")
nyeri_df <- kenya_counties_sf |> filter(County %in% nyeri)
headquarters_nyeri_sf <- headquarters_sf |> filter(County %in% nyeri)

map_nyeri <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% nyeri) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nyeri

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_019_nyeri_canva.png", width = 3, height = 3, dpi = 300)

map_nyeri_zoom <- ggplot(data = nyeri_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_nyeri_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nyeri_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_019_nyeri_canva.png", width = 3, height = 3, dpi = 300)

# County 020 - Kirinyaga

kirinyaga <- c("Kirinyaga")
kirinyaga_df <- kenya_counties_sf |> filter(County %in% kirinyaga)
headquarters_kirinyaga_sf <- headquarters_sf |> filter(County %in% kirinyaga)

map_kirinyaga <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kirinyaga) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kirinyaga

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_020_kirinyaga_canva.png", width = 3, height = 3, dpi = 300)

map_kirinyaga_zoom <- ggplot(data = kirinyaga_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kirinyaga_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kirinyaga_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_020_kirinyaga_canva.png", width = 3, height = 3, dpi = 300)

# County 021 - Murang'a

muranga <- c("Murang'a")
muranga_df <- kenya_counties_sf |> filter(County %in% muranga)
headquarters_muranga_sf <- headquarters_sf |> filter(County %in% muranga)

map_muranga <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% muranga) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_muranga

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_021_muranga_canva.png", width = 3, height = 3, dpi = 300)

map_muranga_zoom <- ggplot(data = muranga_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_muranga_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_muranga_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_021_muranga_canva.png", width = 3, height = 3, dpi = 300)

# County 022 - Kiambu

kiambu <- c("Kiambu")
kiambu_df <- kenya_counties_sf |> filter(County %in% kiambu)
headquarters_kiambu_sf <- headquarters_sf |> filter(County %in% kiambu)

map_kiambu <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kiambu) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kiambu

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_022_kiambu_canva.png", width = 3, height = 3, dpi = 300)

map_kiambu_zoom <- ggplot(data = kiambu_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kiambu_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kiambu_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_022_kiambu_canva.png", width = 3, height = 3, dpi = 300)

# County 023 - Turkana

turkana <- c("Turkana")
turkana_df <- kenya_counties_sf |> filter(County %in% turkana)
headquarters_turkana_sf <- headquarters_sf |> filter(County %in% turkana)

map_turkana <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% turkana) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_turkana

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_023_turkana_canva.png", width = 3, height = 3, dpi = 300)

map_turkana_zoom <- ggplot(data = turkana_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_turkana_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_turkana_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_023_turkana_canva.png", width = 3, height = 3, dpi = 300)

# County 024 - West Pokot

west_pokot <- c("West Pokot")
west_pokot_df <- kenya_counties_sf |> filter(County %in% west_pokot)
headquarters_west_pokot_sf <- headquarters_sf |> filter(County %in% west_pokot)

map_west_pokot <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% west_pokot) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_west_pokot

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_024_west_pokot_canva.png", width = 3, height = 3, dpi = 300)

map_west_pokot_zoom <- ggplot(data = west_pokot_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_west_pokot_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_west_pokot_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_024_west_pokot_canva.png", width = 3, height = 3, dpi = 300)

# County 025 - Samburu

samburu <- c("Samburu")
samburu_df <- kenya_counties_sf |> filter(County %in% samburu)
headquarters_samburu_sf <- headquarters_sf |> filter(County %in% samburu)

map_samburu <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% samburu) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_samburu

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_025_samburu_canva.png", width = 3, height = 3, dpi = 300)

map_samburu_zoom <- ggplot(data = samburu_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_samburu_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_samburu_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_025_samburu_canva.png", width = 3, height = 3, dpi = 300)

# County 026 - Trans Nzoia

trans_nzoia <- c("Trans Nzoia")
trans_nzoia_df <- kenya_counties_sf |> filter(County %in% trans_nzoia)
headquarters_trans_nzoia_sf <- headquarters_sf |> filter(County %in% trans_nzoia)

map_trans_nzoia <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% trans_nzoia) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_trans_nzoia

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_026_trans_nzoia_canva.png", width = 3, height = 3, dpi = 300)

map_trans_nzoia_zoom <- ggplot(data = trans_nzoia_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_trans_nzoia_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_trans_nzoia_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_026_trans_nzoia_canva.png", width = 3, height = 3, dpi = 300)

# County 027 - Uasin Gishu

uasin_gishu <- c("Uasin Gishu")
uasin_gishu_df <- kenya_counties_sf |> filter(County %in% uasin_gishu)
headquarters_uasin_gishu_sf <- headquarters_sf |> filter(County %in% uasin_gishu)

map_uasin_gishu <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% uasin_gishu) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_uasin_gishu

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_027_uasin_gishu_canva.png", width = 3, height = 3, dpi = 300)

map_uasin_gishu_zoom <- ggplot(data = uasin_gishu_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_uasin_gishu_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_uasin_gishu_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_027_uasin_gishu_canva.png", width = 3, height = 3, dpi = 300)

# County 028 - Elgeyo Marakwet

elgeyo_marakwet <- c("Elgeyo Marakwet")
elgeyo_marakwet_df <- kenya_counties_sf |> filter(County %in% elgeyo_marakwet)
headquarters_elgeyo_marakwet_sf <- headquarters_sf |> filter(County %in% elgeyo_marakwet)

map_elgeyo_marakwet <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% elgeyo_marakwet) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_elgeyo_marakwet

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_028_elgeyo_marakwet_canva.png", width = 3, height = 3, dpi = 300)

map_elgeyo_marakwet_zoom <- ggplot(data = elgeyo_marakwet_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_elgeyo_marakwet_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_elgeyo_marakwet_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_028_elgeyo_marakwet_canva.png", width = 3, height = 3, dpi = 300)

# County 029 - Nandi

nandi <- c("Nandi")
nandi_df <- kenya_counties_sf |> filter(County %in% nandi)
headquarters_nandi_sf <- headquarters_sf |> filter(County %in% nandi)

map_nandi <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% nandi) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nandi

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_029_nandi_canva.png", width = 3, height = 3, dpi = 300)

map_nandi_zoom <- ggplot(data = nandi_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_nandi_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nandi_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_029_nandi_canva.png", width = 3, height = 3, dpi = 300)

# County 030 - Baringo

baringo <- c("Baringo")
baringo_df <- kenya_counties_sf |> filter(County %in% baringo)
headquarters_baringo_sf <- headquarters_sf |> filter(County %in% baringo)

map_baringo <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% baringo) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_baringo

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_030_baringo_canva.png", width = 3, height = 3, dpi = 300)

map_baringo_zoom <- ggplot(data = baringo_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_baringo_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_baringo_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_030_baringo_canva.png", width = 3, height = 3, dpi = 300)

# County 031 - Laikipia

laikipia <- c("Laikipia")
laikipia_df <- kenya_counties_sf |> filter(County %in% laikipia)
headquarters_laikipia_sf <- headquarters_sf |> filter(County %in% laikipia)

map_laikipia <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% laikipia) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_laikipia

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_031_laikipia_canva.png", width = 3, height = 3, dpi = 300)

map_laikipia_zoom <- ggplot(data = laikipia_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_laikipia_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_laikipia_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_031_laikipia_canva.png", width = 3, height = 3, dpi = 300)

# County 032 - Nakuru

nakuru <- c("Nakuru")
nakuru_df <- kenya_counties_sf |> filter(County %in% nakuru)
headquarters_nakuru_sf <- headquarters_sf |> filter(County %in% nakuru)

map_nakuru <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% nakuru) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nakuru

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_032_nakuru_canva.png", width = 3, height = 3, dpi = 300)

map_nakuru_zoom <- ggplot(data = nakuru_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_nakuru_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nakuru_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_032_nakuru_canva.png", width = 3, height = 3, dpi = 300)

# County 033 - Narok

narok <- c("Narok")
narok_df <- kenya_counties_sf |> filter(County %in% narok)
headquarters_narok_sf <- headquarters_sf |> filter(County %in% narok)

map_narok <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% narok) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_narok

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_033_narok_canva.png", width = 3, height = 3, dpi = 300)

map_narok_zoom <- ggplot(data = narok_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_narok_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_narok_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_033_narok_canva.png", width = 3, height = 3, dpi = 300)

# County 034 - Kajiado

kajiado <- c("Kajiado")
kajiado_df <- kenya_counties_sf |> filter(County %in% kajiado)
headquarters_kajiado_sf <- headquarters_sf |> filter(County %in% kajiado)

map_kajiado <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kajiado) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kajiado

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_034_kajiado_canva.png", width = 3, height = 3, dpi = 300)

map_kajiado_zoom <- ggplot(data = kajiado_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kajiado_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kajiado_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_034_kajiado_canva.png", width = 3, height = 3, dpi = 300)

# County 035 - Kericho

kericho <- c("Kericho")
kericho_df <- kenya_counties_sf |> filter(County %in% kericho)
headquarters_kericho_sf <- headquarters_sf |> filter(County %in% kericho)

map_kericho <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kericho) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kericho

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_035_kericho_canva.png", width = 3, height = 3, dpi = 300)

map_kericho_zoom <- ggplot(data = kericho_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kericho_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kericho_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_035_kericho_canva.png", width = 3, height = 3, dpi = 300)

# County 036 - Bomet

bomet <- c("Bomet")
bomet_df <- kenya_counties_sf |> filter(County %in% bomet)
headquarters_bomet_sf <- headquarters_sf |> filter(County %in% bomet)

map_bomet <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% bomet) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_bomet

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_036_bomet_canva.png", width = 3, height = 3, dpi = 300)

map_bomet_zoom <- ggplot(data = bomet_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_bomet_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_bomet_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_036_bomet_canva.png", width = 3, height = 3, dpi = 300)

# County 037 - Kakamega

kakamega <- c("Kakamega")
kakamega_df <- kenya_counties_sf |> filter(County %in% kakamega)
headquarters_kakamega_sf <- headquarters_sf |> filter(County %in% kakamega)

map_kakamega <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kakamega) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kakamega

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_037_kakamega_canva.png", width = 3, height = 3, dpi = 300)

map_kakamega_zoom <- ggplot(data = kakamega_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kakamega_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kakamega_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_037_kakamega_canva.png", width = 3, height = 3, dpi = 300)

# County 038 - Vihiga

vihiga <- c("Vihiga")
vihiga_df <- kenya_counties_sf |> filter(County %in% vihiga)
headquarters_vihiga_sf <- headquarters_sf |> filter(County %in% vihiga)

map_vihiga <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% vihiga) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_vihiga

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_038_vihiga_canva.png", width = 3, height = 3, dpi = 300)

map_vihiga_zoom <- ggplot(data = vihiga_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_vihiga_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_vihiga_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_038_vihiga_canva.png", width = 3, height = 3, dpi = 300)

# County 039 - Bungoma

bungoma <- c("Bungoma")
bungoma_df <- kenya_counties_sf |> filter(County %in% bungoma)
headquarters_bungoma_sf <- headquarters_sf |> filter(County %in% bungoma)

map_bungoma <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% bungoma) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_bungoma

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_039_bungoma_canva.png", width = 3, height = 3, dpi = 300)

map_bungoma_zoom <- ggplot(data = bungoma_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_bungoma_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_bungoma_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_039_bungoma_canva.png", width = 3, height = 3, dpi = 300)

# County 040 - Busia

busia <- c("Busia")
busia_df <- kenya_counties_sf |> filter(County %in% busia)
headquarters_busia_sf <- headquarters_sf |> filter(County %in% busia)

map_busia <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% busia) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_busia

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_040_busia_canva.png", width = 3, height = 3, dpi = 300)

map_busia_zoom <- ggplot(data = busia_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_busia_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_busia_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_040_busia_canva.png", width = 3, height = 3, dpi = 300)

# County 041 - Siaya

siaya <- c("Siaya")
siaya_df <- kenya_counties_sf |> filter(County %in% siaya)
headquarters_siaya_sf <- headquarters_sf |> filter(County %in% siaya)

map_siaya <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% siaya) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_siaya

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_041_siaya_canva.png", width = 3, height = 3, dpi = 300)

map_siaya_zoom <- ggplot(data = siaya_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_siaya_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_siaya_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_041_siaya_canva.png", width = 3, height = 3, dpi = 300)

# County 042 - Kisumu

kisumu <- c("Kisumu")
kisumu_df <- kenya_counties_sf |> filter(County %in% kisumu)
headquarters_kisumu_sf <- headquarters_sf |> filter(County %in% kisumu)

map_kisumu <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kisumu) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kisumu

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_042_kisumu_canva.png", width = 3, height = 3, dpi = 300)

map_kisumu_zoom <- ggplot(data = kisumu_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kisumu_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kisumu_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_042_kisumu_canva.png", width = 3, height = 3, dpi = 300)

# County 043 - Homa Bay

homa_bay <- c("Homa Bay")
homa_bay_df <- kenya_counties_sf |> filter(County %in% homa_bay)
headquarters_homa_bay_sf <- headquarters_sf |> filter(County %in% homa_bay)

map_homa_bay <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% homa_bay) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_homa_bay

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_043_homa_bay_canva.png", width = 3, height = 3, dpi = 300)

map_homa_bay_zoom <- ggplot(data = homa_bay_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_homa_bay_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_homa_bay_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_043_homa_bay_canva.png", width = 3, height = 3, dpi = 300)

# County 044 - Migori

migori <- c("Migori")
migori_df <- kenya_counties_sf |> filter(County %in% migori)
headquarters_migori_sf <- headquarters_sf |> filter(County %in% migori)

map_migori <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% migori) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_migori

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_044_migori_canva.png", width = 3, height = 3, dpi = 300)

map_migori_zoom <- ggplot(data = migori_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_migori_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_migori_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_044_migori_canva.png", width = 3, height = 3, dpi = 300)

# County 045 - Kisii

kisii <- c("Kisii")
kisii_df <- kenya_counties_sf |> filter(County %in% kisii)
headquarters_kisii_sf <- headquarters_sf |> filter(County %in% kisii)

map_kisii <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% kisii) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kisii

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_045_kisii_canva.png", width = 3, height = 3, dpi = 300)

map_kisii_zoom <- ggplot(data = kisii_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_kisii_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kisii_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_045_kisii_canva.png", width = 3, height = 3, dpi = 300)

# County 046 - Nyamira

nyamira <- c("Nyamira")
nyamira_df <- kenya_counties_sf |> filter(County %in% nyamira)
headquarters_nyamira_sf <- headquarters_sf |> filter(County %in% nyamira)

map_nyamira <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% nyamira) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nyamira

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_046_nyamira_canva.png", width = 3, height = 3, dpi = 300)

map_nyamira_zoom <- ggplot(data = nyamira_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_nyamira_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nyamira_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_046_nyamira_canva.png", width = 3, height = 3, dpi = 300)

# County 047 - Nairobi City

nairobi_city <- c("Nairobi City")
nairobi_city_df <- kenya_counties_sf |> filter(County %in% nairobi_city)
headquarters_nairobi_city_sf <- headquarters_sf |> filter(County %in% nairobi_city)

map_nairobi_city <- ggplot(data = kenya_counties_sf) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  scale_fill_manual(values = "goldenrod2") +
  gghighlight(County %in% nairobi_city) +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nairobi_city

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_047_nairobi_city_canva.png", width = 3, height = 3, dpi = 300)

map_nairobi_city_zoom <- ggplot(data = nairobi_city_df) +
  geom_sf(aes(fill = County), linewidth = 0.1) +
  geom_sf(data = headquarters_nairobi_city_sf, color = "aquamarine4", size = 2) +
  scale_fill_manual(values = "goldenrod2") +
  theme_void() +
  labs(title = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nairobi_city_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/map_zoom_047_nairobi_city_canva.png", width = 3, height = 3, dpi = 300)

