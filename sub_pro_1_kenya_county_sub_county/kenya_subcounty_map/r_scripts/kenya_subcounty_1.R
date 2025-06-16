# Kenya Subcounty Analysis
# Census data analyzed at the subcounty level

## Load the required libraries

library(ggplot2)
library(sf)
library(rKenyaCensus)
library(janitor)
library(tidyverse)

## Load the shapefile and plot the subcounty map

kenya_subcounties <- st_read("sub_pro_1_kenya_county_sub_county/kenya_subcounty_map/shapefiles/ke_subcounty.shp")

ggplot(kenya_subcounties) + 
  geom_sf(fill = "bisque", linewidth = 0.3, color = "black") + 
  theme_void()

# Clean the data

kenya_subcounties$subcounty <- gsub("Sub County", "", kenya_subcounties$subcounty)
kenya_subcounties$subcounty <- gsub("sub-county", "", kenya_subcounties$subcounty)
kenya_subcounties$subcounty <- gsub("Sub- County", "", kenya_subcounties$subcounty)
kenya_subcounties$subcounty <- str_trim(kenya_subcounties$subcounty) 

kenya_subcounties$subcounty[str_detect(kenya_subcounties$subcounty, "ounty")]

# Get census data

df <- V4_T1.9

df_clean <- df %>%
  clean_names()

df_clean_new <- df_clean %>%
  mutate(county = tools::toTitleCase(tolower(county))) |>
  mutate(sub_county = tools::toTitleCase(tolower(sub_county))) |>
  rename(subcounty = sub_county)

df_clean_new$subcounty[str_detect(df_clean_new$subcounty, "  ")]

########################################################################
# FIND MISMATCHES
########################################################################

setdiff(kenya_subcounties$subcounty, df_clean_new$subcounty)

setdiff(df_clean_new$subcounty, kenya_subcounties$subcounty)

mismatch <- full_join(kenya_subcounties, df_clean_new, by = "subcounty")




ggplot(kenya_counties) + 
  geom_sf(fill = "bisque", linewidth = 1, color = "black") + 
  theme_void()