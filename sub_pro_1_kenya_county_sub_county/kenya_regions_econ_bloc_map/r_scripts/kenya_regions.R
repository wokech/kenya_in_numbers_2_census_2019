# Regions of Kenya
# By @kenya.in.numbers
# Intro maps of the Regions of Kenya
# From the Kenya Population and Housing Census Report (2019) and rKenyaCensus

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
library(ggsci)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Plot a void map of Kenya
full_map_kenya <- ggplot(kenya_counties_sf) + 
  geom_sf(fill = 'goldenrod3', linewidth = 0.6, color = "black") + 
  theme_void() +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

full_map_kenya

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/full_map_kenya.png", width = 12, height = 12, dpi = 300)

#) Change the title case of the counties and fix the names

unique(kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

#####################
#####PART B - Visualize Data
#####################

# Listing of the Regions of Kenya and segmenting of datasets by region

# Coast Region - 6 counties
coast <- c("Mombasa", "Kwale", "Kilifi", "Tana River", "Lamu", "Taita Taveta")
coast_df <- kenya_counties_sf |> filter(County %in% coast)
  
map_coast <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% coast) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_coast

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/coast_map.png", width = 12, height = 12, dpi = 300)

map_coast_zoom <- ggplot(data = coast_df)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = County), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_coast_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/coast_map_zoom.png", width = 12, height = 12, dpi = 300)

# North Eastern Region - 3 counties
north_eastern <- c("Garissa", "Wajir", "Mandera")
north_eastern_df <- kenya_counties_sf |> filter(County %in% north_eastern)

map_north_eastern <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% north_eastern) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_north_eastern

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/north_eastern_map.png", width = 12, height = 12, dpi = 300)

map_north_eastern_zoom <- ggplot(data = north_eastern_df)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = County), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_north_eastern_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/north_eastern_map_zoom.png", width = 12, height = 12, dpi = 300)

# Eastern Region - 8 counties
eastern <- c("Marsabit", "Isiolo", "Meru", "Tharaka Nithi", "Embu", "Kitui", "Machakos", "Makueni")
eastern_df <- kenya_counties_sf |> filter(County %in% eastern)

map_eastern <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% eastern) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_eastern

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/eastern_map.png", width = 12, height = 12, dpi = 300)

map_eastern_zoom <- ggplot(data = eastern_df)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = County), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_eastern_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/eastern_map_zoom.png", width = 12, height = 12, dpi = 300)

# Central Region - 5 counties
central <- c("Nyeri", "Kirinyaga", "Kiambu", "Murang’a", "Nyandarua")
central_df <- kenya_counties_sf |> filter(County %in% central)

map_central <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% central) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_central

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/central_map.png", width = 12, height = 12, dpi = 300)

map_central_zoom <- ggplot(data = central_df)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = County), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_central_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/central_map_zoom.png", width = 12, height = 12, dpi = 300)

# Rift Valley Region - 14 counties
rift_valley <- c("Turkana", "West Pokot", "Samburu", "Trans Nzoia", "Uasin Gishu", 
                 "Elgeyo Marakwet", "Nandi", "Baringo", "Laikipia", "Nakuru",
                 "Narok", "Kajiado", "Kericho", "Bomet")
rift_valley_df <- kenya_counties_sf |> filter(County %in% rift_valley)

map_rift_valley <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% rift_valley) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_rift_valley

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/rift_valley_map.png", width = 12, height = 12, dpi = 300)

map_rift_valley_zoom <- ggplot(data = rift_valley_df)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = County), size = 10,
                     force = 100, nudge_x = -2, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_rift_valley_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/rift_valley_map_zoom.png", width = 12, height = 12, dpi = 300)

# Western Region - 4 counties
western <- c("Kakamega", "Vihiga", "Bungoma", "Busia")
western_df <- kenya_counties_sf |> filter(County %in% western)

map_western <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% western) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_western

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/western_map.png", width = 12, height = 12, dpi = 300)

map_western_zoom <- ggplot(data = western_df)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = County), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_western_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/western_map_zoom.png", width = 12, height = 12, dpi = 300)

# Nyanza Region - 6 counties
nyanza <- c("Siaya", "Homa Bay", "Kisumu", "Kisii", "Nyamira", "Migori")
nyanza_df <- kenya_counties_sf |> filter(County %in% nyanza)

map_nyanza <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% nyanza) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_nyanza

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/nyanza_map.png", width = 12, height = 12, dpi = 300)

map_nyanza_zoom <- ggplot(data = nyanza_df)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = County), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_nyanza_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/nyanza_map_zoom.png", width = 12, height = 12, dpi = 300)

# Nairobi Region - 1 county
nairobi <- c("Nairobi City")
nairobi_df <- kenya_counties_sf |> filter(County %in% nairobi)

map_nairobi <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% nairobi) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_nairobi

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/nairobi_map.png", width = 12, height = 12, dpi = 300)

map_nairobi_zoom <- ggplot(data = nairobi_df)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = County), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

map_nairobi_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/regions/nairobi_map_zoom.png", width = 12, height = 12, dpi = 300)