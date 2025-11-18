# KNBS Gross County Product Analysis 2024
# Poverty Estimates Map
# By @kenya.in.numbers
# From GCP 2024

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


# 1) Load the required data

# Poverty Estimates (2015 - 2022)
poverty_estimates <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                           "datasets", "kenya_gcp_2024_tables",
                                           "poverty_estimates.xlsx"))


poverty_estimates <- poverty_estimates |>
  clean_names() |>
  filter(residence_county != c("NATIONAL", "RURAL", "URBAN")) |>
  select(residence_county, x2022_percent)

# 2) Generate the maps using shapefiles and sf package

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
unique(poverty_estimates$residence_county)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names


# Inspect the county names that are different in each of the datasets
unique(poverty_estimates$residence_county)[which(!unique(poverty_estimates$residence_county) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, poverty_estimates, by = c("County" = "residence_county"))

# Fix the county names

merged_df$County <- gsub("/", " ", merged_df$County)
merged_df$County <- gsub("-", " ", merged_df$County)

### Convert the m_f_ratio county names to title case
merged_df <- merged_df |> 
  mutate(County = tools::toTitleCase(tolower(County)))


# 9) Poverty (%)

map_poverty <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = x2022_percent), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Poverty Incidence (%)")+
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
    limits = c(15, 85)
  ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_poverty

# Save the plot

#ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/poverty_estimates/infographics_1_maps_top_bottom_poverty_estimates.png", width = 12, height = 12, dpi = 300)



# Top 10 Poverty Estimates

poverty_top_10 <- poverty_estimates |>
  select(residence_county, x2022_percent) |>
  top_n(10) |>
  arrange(x2022_percent)

# Bottom 10 Poverty Estimates

poverty_bottom_10 <- poverty_estimates |>
  select(residence_county, x2022_percent) |>
  top_n(-10) |>
  arrange(desc(x2022_percent))
