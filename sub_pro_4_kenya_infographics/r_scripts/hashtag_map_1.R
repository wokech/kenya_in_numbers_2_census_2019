# Kenya Instagram Hashtag Analysis 1
# By @kenya.in.numbers

#####################
#####PART A
#####################

# Load all the required packages and libraries required for accessing the census data

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


# Load the required data

# Insta Hashtags (July 2025)
kenya_insta_hashtags <- read_excel(here::here("sub_pro_4_kenya_infographics", 
                                              "datasets", "kenya_insta_hashtags_1.xlsx"))

kenya_insta_hashtags <- kenya_insta_hashtags |>
  clean_names()

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
unique(kenya_insta_hashtags$county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Rename 3 counties in the sf table
kenya_counties_sf <- kenya_counties_sf |>
  mutate(County = recode(County, "TAITA/TAVETA" = "TAITA TAVETA"),
         County = recode(County, "THARAKA-NITHI" = "THARAKA NITHI"),
         County = recode(County, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET"))

### Convert county names to title case
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Inspect the county names that are different in each of the datasets
unique(kenya_insta_hashtags$county)[which(!unique(kenya_insta_hashtags$county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, kenya_insta_hashtags, by = c("County" = "county"))

#####################
#####PART B
#####################

# 1) Hashtag Map

map_hashtag <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = instagram_hashtag_count_july_2025), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Hashtags")+
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
    limits = c(0, 7500000),
    labels = label_number(scale = 1e-6, suffix = "M")
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_hashtag

# Save the plot
#ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/map_hashtag.png", width = 12, height = 12, dpi = 300)


# 2) Normalized Hashtags by Population (%) Map

map_hashtag_normalized <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = insta_hashtag_population_percent_july_2025), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Hashtags/Population (%)")+
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
    limits = c(0, 180)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_hashtag_normalized

# Save the plot
ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/map_hashtag_normalized.png", width = 12, height = 12, dpi = 300)
