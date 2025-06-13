# Regional Economic Blocs
# By @kenya.in.numbers
# Intro maps of the Kenyan Regional Economic Blocs
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
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/full_map_kenya.png", width = 12, height = 12, dpi = 300)

#) Change the title case of the counties and fix the names

unique(kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

#####################
#####PART B - Visualize Data
#####################

# Listing of regional economic blocs and segmenting of datasets by bloc

# Maa Economic Bloc (MEB) - 3 counties
meb <- c("Narok", "Kajiado", "Samburu")
meb_df <- kenya_counties_sf |> filter(County %in% meb)

map_meb <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% meb) +
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

map_meb

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/meb_map.png", width = 12, height = 12, dpi = 300)

map_meb_zoom <- ggplot(data = meb_df)+
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

map_meb_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/meb_map_zoom.png", width = 12, height = 12, dpi = 300)


# Frontier Counties Development Council (FCDC) - 9 counties
fcdc <- c("Garissa", "Wajir", "Mandera", 
          "Isiolo", "Marsabit", "Tana River", 
          "Lamu", "West Pokot", "Turkana",
          "Samburu")

fcdc_df <- kenya_counties_sf |> filter(County %in% fcdc)

map_fcdc <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% fcdc) +
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

map_fcdc

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/fcdc_map.png", width = 12, height = 12, dpi = 300)

map_fcdc_zoom <- ggplot(data = fcdc_df)+
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

map_fcdc_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/fcdc_map_zoom.png", width = 12, height = 12, dpi = 300)

# North Rift Economic Bloc (NOREB) - 8 counties
noreb <- c("Uasin Gishu", "Trans Nzoia", "Nandi", 
           "Elgeyo Marakwet", "West Pokot", "Baringo", 
           "Samburu", "Turkana")

noreb_df <- kenya_counties_sf |> filter(County %in% noreb)

map_noreb <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% noreb) +
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

map_noreb

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/noreb_map.png", width = 12, height = 12, dpi = 300)

map_noreb_zoom <- ggplot(data = noreb_df)+
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

map_noreb_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/noreb_map_zoom.png", width = 12, height = 12, dpi = 300)

# Lake Region Economic Bloc (LREB) - 14 counties
lreb <- c("Migori", "Nyamira", "Siaya", 
          "Vihiga", "Bomet", "Bungoma", 
          "Busia", "Homa Bay", "Kakamega", 
          "Kisii", "Kisumu", "Nandi", 
          "Trans Nzoia", "Kericho")

lreb_df <- kenya_counties_sf |> filter(County %in% lreb)

map_lreb <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% lreb) +
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

map_lreb

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/lreb_map.png", width = 12, height = 12, dpi = 300)

map_lreb_zoom <- ggplot(data = lreb_df)+
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

map_lreb_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/lreb_map_zoom.png", width = 12, height = 12, dpi = 300)

# South Eastern Kenya Economic Bloc (SEKEB) - 3 counties
sekeb <- c("Kitui", "Machakos", "Makueni")

sekeb_df <- kenya_counties_sf |> filter(County %in% sekeb)

map_sekeb <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% sekeb) +
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

map_sekeb

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/sekeb_map.png", width = 12, height = 12, dpi = 300)

map_sekeb_zoom <- ggplot(data = sekeb_df)+
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

map_sekeb_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/sekeb_map_zoom.png", width = 12, height = 12, dpi = 300)

# Mt Kenya and Aberdare’s Economic Bloc 
mkareb <- c("Nyeri", "Nyandarua", "Meru", 
            "Tharaka Nithi", "Embu", "Kirinyaga", 
            "Murang'a", "Laikipia", "Nakuru", "Kiambu")

mkareb_df <- kenya_counties_sf |> filter(County %in% mkareb)

map_mkareb <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% mkareb) +
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

map_mkareb

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/mkareb_map.png", width = 12, height = 12, dpi = 300)

map_mkareb_zoom <- ggplot(data = mkareb_df)+
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

map_mkareb_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/mkareb_map_zoom.png", width = 12, height = 12, dpi = 300)

# Jumuiya ya Kaunti za Pwani (JKP) 
jkp <- c("Tana River", "Taita Taveta", "Lamu", 
         "Kilifi", "Kwale", "Mombasa")

jkp_df <- kenya_counties_sf |> filter(County %in% jkp)

map_jkp <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% jkp) +
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

map_jkp

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/jkp_map.png", width = 12, height = 12, dpi = 300)

map_jkp_zoom <- ggplot(data = jkp_df)+
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

map_jkp_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/jkp_map_zoom.png", width = 12, height = 12, dpi = 300)

# Nairobi Metropolitan Counties
namet <- c("Nairobi City", "Kajiado", "Murang'a", 
           "Kiambu", "Machakos")

namet_df <- kenya_counties_sf |> filter(County %in% namet)

map_namet <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% namet) +
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

map_namet

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/namet_map.png", width = 12, height = 12, dpi = 300)

map_namet_zoom <- ggplot(data = namet_df)+
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

map_namet_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/namet_map_zoom.png", width = 12, height = 12, dpi = 300)

# Kenyan Cities
kenya_city <- c("Nairobi City", "Mombasa", "Kisumu", 
                "Nakuru", "Uasin Gishu")

kenya_city_df <- kenya_counties_sf |> filter(County %in% kenya_city)

map_kenya_city <- ggplot(data = kenya_counties_sf)+
  geom_sf(aes(fill = County), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(County %in% kenya_city) +
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

map_kenya_city

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/kenya_city_map.png", width = 12, height = 12, dpi = 300)

map_kenya_city_zoom <- ggplot(data = kenya_city_df)+
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

map_kenya_city_zoom

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_regions_econ_bloc_map/images/econ_blocs/kenya_city_map_zoom.png", width = 12, height = 12, dpi = 300)

