# Mapping of the Infographic 3 Datasets
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
                                        "datasets", "infographic_data_3.csv"))

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


# 1) DeskTop/Computer/Laptop/Tablet Map

map_dclt <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = desk_top_computer_laptop_tablet), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households with a Personal Computing Device (%) -\n(Desktop/Computer/Laptop/Tablet)")+
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

map_dclt

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_3_maps_top_bottom/map_dclt.png", width = 12, height = 12, dpi = 300)



# 2) Bicycle Map

map_bicycle <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = bicycle), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households with a bicycle (%)")+
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

map_bicycle

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_3_maps_top_bottom/map_bicycle.png", width = 12, height = 12, dpi = 300)



# 3) Motor Cycle Map

map_motor_cycle <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = motor_cycle), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households with a motorcycle (%)")+
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
    limits = c(0, 16)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_motor_cycle

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_3_maps_top_bottom/map_motor_cycle.png", width = 12, height = 12, dpi = 300)



# 4) Refrigerator Map

map_refrigerator <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = refrigerator), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households with a refrigerator (%)")+
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

map_refrigerator

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_3_maps_top_bottom/map_refrigerator.png", width = 12, height = 12, dpi = 300)


# 5) Truck / Lorry / Bus / Three-Wheeler Truck Map

map_truck_lorry_bus_three_wheelertruck <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = truck_lorry_bus_three_wheelertruck), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households with a commercial vehicle (%) -\n(Truck/Lorry/Bus/Three-Wheeler)")+
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
    limits = c(0, 2)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_truck_lorry_bus_three_wheelertruck

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_3_maps_top_bottom/map_truck_lorry_bus_three_wheelertruck.png", width = 12, height = 12, dpi = 300)




# 6) Tuk Tuk Map

map_tuk_tuk <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = tuk_tuk), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households with a Tuk Tuk (%)")+
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
    limits = c(0, 3)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_tuk_tuk

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/infographics_3_maps_top_bottom/map_tuk_tuk.png", width = 12, height = 12, dpi = 300)


