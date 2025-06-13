# Kenya Subcounty Analysis
# Census data analyzed at the subcounty level

## Load the required libraries

library(ggplot2)
library(sf)
library(rKenyaCensus)
library(janitor)

## Load the shapefile and plot the subcounty map

kenya_subcounties <- st_read("sub_pro_6_kenya_subcounty_analysis/shapefiles/ke_subcounty.shp")

ggplot(kenya_subcounties) + 
  geom_sf(fill = "bisque", linewidth = 1, color = "black") + 
  theme_void()

# Remove all references to sub county

kenya_subcounties$subcounty <- gsub("Sub County", "", kenya_subcounties$subcounty)
kenya_subcounties$subcounty <- gsub("sub-county", "", kenya_subcounties$subcounty)
kenya_subcounties$subcounty <- gsub("Sub- County", "", kenya_subcounties$subcounty)

kenya_subcounties %>% 
  filter(str_detect(subcounty, "ounty"))

# String to Upper

kenya_subcounties$subcounty <- toupper(kenya_subcounties$subcounty)


df <- V3_T2.3

df_clean <- df %>%
  clean_names()

df_clean_new <- df_clean %>%
  filter(sub_county !=  "ALL") %>%
  filter(age == "Total") %>%
  rename("subcounty" = "sub_county")

df_clean_new$subcounty <- gsub("\\s+$", "", df_clean_new$subcounty)
kenya_subcounties$subcounty <- gsub("\\s+$", "", kenya_subcounties$subcounty)

########################################################################
# FIND MISMATCHES
########################################################################

aa <- anti_join(kenya_subcounties, df_clean_new, by = "subcounty")
bb <- left_join(kenya_subcounties, df_clean_new, by = "subcounty")

## Merge datasets

kenya_sub_df_new <- left_join(kenyan_subcounties, )

ggplot(kenya_counties) + 
  geom_sf(fill = "bisque", linewidth = 1, color = "black") + 
  theme_void()