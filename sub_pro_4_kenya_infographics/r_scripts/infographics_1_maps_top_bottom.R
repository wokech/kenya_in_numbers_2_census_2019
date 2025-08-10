# Mapping of the Infographic Datasets
# By @kenya.in.numbers

# Data: Kenya GCP (2024)

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
                                            "datasets", "infographic_data_1.csv"))

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
  mutate(County = tools::toTitleCase(tolower(County)))

#####################
#####PART B
#####################

# 1) Male-Female Ratio Map

map_m_f_ratio <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = M_F_Ratio_per_100), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
         fill = "Ratio of Males to Females")+
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
    limits = c(90, 120)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_m_f_ratio

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_m_f_100.png", width = 12, height = 12, dpi = 300)

# 2) Number of Households

map_number_hh <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = NumberOfHouseholds), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Households")+
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
    limits = c(35000, 1510000),
    trans = "log10",
    labels = scales::label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_number_hh

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_number_hh.png", width = 12, height = 12, dpi = 300)


# 3) Average Household Size

map_avg_hh_size <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = AverageHouseholdSize), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Average Household Size")+
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
    limits = c(2.8, 7)
  ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_avg_hh_size

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_avg_hh_size.png", width = 12, height = 12, dpi = 300)



# 4) Population

map_pop <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = Population.y), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Population")+
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
    limits = c(140000, 4400000),
    trans = "log10",
    labels = scales::label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_pop

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_pop.png", width = 12, height = 12, dpi = 300)


# 5) Population Density

map_pop_density <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = `Population Density(No. per Sq. Km)`), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Population Density (Per Square Kilometre)")+
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
    limits = c(5, 6300),
    trans = "log10",
    labels = scales::label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_pop_density

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_pop_density.png", width = 12, height = 12, dpi = 300)



# 6) Mobile Phone Ownership (%)

map_mpo <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = MPO_Total_Perc), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Mobile Phone Ownership (%)")+
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
    limits = c(16, 70)
  ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_mpo

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_mpo.png", width = 12, height = 12, dpi = 300)



# 7) Use of Internet (%)

map_uoi <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = UoI_Total_Perc), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Internet Usage (%)")+
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
    limits = c(6, 53)
  ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_uoi

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_uoi.png", width = 12, height = 12, dpi = 300)



# 8) Radio Ownership

map_radio <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = StandAloneRadio), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Radio Ownership (%)")+
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
    limits = c(12, 75)
  ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_radio

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_radio.png", width = 12, height = 12, dpi = 300)



# 9) TV Ownership

map_tv <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = FunctionalTV), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "TV Ownership (%)")+
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
    limits = c(6, 70)
  ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_tv

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_tv.png", width = 12, height = 12, dpi = 300)



# 9) Car Ownership (%)

map_car <- ggplot(data = merged_df_infographics)+
  geom_sf(aes(geometry = geometry, fill = Car), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Car Ownership (%)")+
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
    limits = c(1, 13)
  ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_car

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/infographics_1_maps_top_bottom/map_car.png", width = 12, height = 12, dpi = 300)

