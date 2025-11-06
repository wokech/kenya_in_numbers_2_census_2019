# Mapping of the Infographic 4 Datasets
# By @kenya.in.numbers

# Data: Census(2019)

#####################
#####PART A
#####################

# 1) Load all the required packages and libraries

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

# 2) Load the infographic data
infographic_data <- read_csv(here::here("sub_pro_4_kenya_infographics", 
                                        "datasets", "infographic_data_4.csv"))

infographic_data <- infographic_data |>
  rename(County = county)

# Remove the Kenya data
infographic_data_no_total <- infographic_data |>
  filter(County != "Kenya")

# 5) Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Plot a void map of Kenya
ggplot(kenya_counties_sf) + 
  geom_sf() + 
  theme_void()

# 6) Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(infographic_data_no_total$County)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
infographic_data_no_total$County <- toupper(infographic_data_no_total$County)

# Inspect the county names that are different in each of the datasets
unique(infographic_data_no_total$County)[which(!unique(infographic_data_no_total$County) %in% kenya_counties_sf$County)]

# Rename 3 counties in the sf table
kenya_counties_sf <- kenya_counties_sf |>
  mutate(County = recode(County, "TAITA/TAVETA" = "TAITA TAVETA"),
         County = recode(County, "THARAKA-NITHI" = "THARAKA NITHI"),
         County = recode(County, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET"))

# Merge the two datasets for ease of plotting
merged_df_infographics <- left_join(kenya_counties_sf, infographic_data_no_total, by = "County")

### Convert the m_f_ratio county names to title case
merged_df_infographics <- merged_df_infographics |> 
  mutate(County = tools::toTitleCase(tolower(County))) |>
  clean_names()

#####################
#####PART B
#####################


# 1) Piped Water to Compound Map

map_total_piped <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = total_piped), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households with Piped Water to Compound (%)")+
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
    limits = c(0, 65)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_total_piped

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_total_piped.png", width = 12, height = 12, dpi = 300)

# 2) Open Bush Defecation Map

map_open_bush <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = open_bush), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households practicing open bush defecation (%)")+
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
    limits = c(0, 70)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_open_bush

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_open_bush.png", width = 12, height = 12, dpi = 300)

# 3) Flushing Toilet Map

map_flush_toilet <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = total_flush), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households with a flushing toilet (sewer/septic) (%)")+
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

map_flush_toilet

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_flush_toilet.png", width = 12, height = 12, dpi = 300)

# 4) LPG + Biogas Map

map_lpg_biogas <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = total_gas), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households cooking with LPG and Biogas (%)")+
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
    limits = c(0, 70)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_lpg_biogas

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_lpg_biogas.png", width = 12, height = 12, dpi = 300)


# 5) Firewood + Charcoal Map

map_total_wood <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = total_wood), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households cooking with firewood and charcoal (%)")+
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
    limits = c(0, 100)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_total_wood

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_total_wood.png", width = 12, height = 12, dpi = 300)


# 6) Mains Electricity Map

map_mains_electricity <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = mains_electricity), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households using mains electricity for lighting (%)")+
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
    limits = c(0, 100)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_mains_electricity

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_mains_electricity.png", width = 12, height = 12, dpi = 300)


# 7) Solar Map

map_solar <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = solar), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households using solar for lighting (%)")+
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
    limits = c(0, 55)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_solar

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_solar.png", width = 12, height = 12, dpi = 300)


# 8) Torch for Lighting Map

map_total_torch <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = total_torch), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households using torches (solar- or battery-powered) for lighting (%)")+
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
    limits = c(0, 55)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_total_torch

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_total_torch.png", width = 12, height = 12, dpi = 300)


# 9) Paraffin for Lighting Map

map_total_paraffin <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = total_paraffin), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households using paraffin fuel for lighting (%)")+
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

map_total_paraffin

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_4_maps_top_bottom/map_total_paraffin.png", width = 12, height = 12, dpi = 300)

















