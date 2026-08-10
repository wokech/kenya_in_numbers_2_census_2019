# Area allocated to agriculture

## Land Area (Hectares) ##

# A) Load the required libraries

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(readxl)
library(janitor)
library(scales) # control axis/scale format
library(devtools)
#devtools::install_github('bbc/bbplot')
#library(bbplot) # plotting theme
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

# B) Load the required datasets

area_ag_sub_comm <- V4_T2.25

#write.csv(area_ag_sub_comm, "sub_pro_7_general_farming/datasets/area_ag_sub_comm.csv")

area_ag_sub_comm <- read.csv("sub_pro_7_general_farming/datasets/area_ag_sub_comm.csv")

# Clean the data
area_ag_sub_comm_clean <- area_ag_sub_comm %>%
  clean_names()

# Create new columns

area_ag_sub_comm_clean_normalized <- area_ag_sub_comm_clean |>
  mutate(land_sub_percent =  (land_size_subsistence*100)/land_size,
         land_comm_percent  = (land_size_commercial*100)/land_size,
         hh_sub_percent  = (no_fhs_subsistence*100)/no_fhs,
         hh_comm_percent = (no_fhs_commercial*100)/no_fhs,
         hh_land_size_total = land_size/no_fhs,
         hh_land_size_sub = land_size_subsistence/no_fhs_subsistence,
         hh_land_size_comm = land_size_commercial/no_fhs_commercial)

# County data
area_ag_sub_comm_clean_normalized_county <- area_ag_sub_comm_clean_normalized %>%
  filter(admin_area == "County" | sub_county == "KENYA")         


################################################################################
# PART B: TOP AND BOTTOM 10 AND NATIONAL AVERAGE
################################################################################

# 1) Subsistence Land (%)

land_sub_percent_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_sub_percent) |>
  top_n(10) |>
  arrange(desc(land_sub_percent))

land_sub_percent_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_sub_percent) |>
  top_n(-10) |>
  arrange(land_sub_percent)

land_sub_percent_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(land_sub_percent)

# 2) Commercial Land (%)

land_comm_percent_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_comm_percent) |>
  top_n(10) |>
  arrange(desc(land_comm_percent))

land_comm_percent_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_comm_percent) |>
  top_n(-10) |>
  arrange(land_comm_percent)

land_comm_percent_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(land_comm_percent)

# 3) Subsistence Land (Hectares)

land_sub_ha_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_size_subsistence) |>
  top_n(10) |>
  arrange(desc(land_size_subsistence))

land_sub_ha_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_size_subsistence) |>
  top_n(-10) |>
  arrange(land_size_subsistence)

land_sub_ha_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(land_size_subsistence)

# 4) Commercial Land (Hectares)

land_comm_ha_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_size_commercial) |>
  top_n(10) |>
  arrange(desc(land_size_commercial))

land_comm_ha_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_size_commercial) |>
  top_n(-10) |>
  arrange(land_size_commercial)

land_comm_ha_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(land_size_commercial)

# 5) Total Farm Land (Hectares)

land_total_ha_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_size) |>
  top_n(10) |>
  arrange(desc(land_size))

land_total_ha_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, land_size) |>
  top_n(-10) |>
  arrange(land_size)

land_total_ha_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(land_size)

# 6) Farm HH (Subsistence)

farm_hh_sub_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, no_fhs_subsistence) |>
  top_n(10) |>
  arrange(desc(no_fhs_subsistence))

farm_hh_sub_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, no_fhs_subsistence) |>
  top_n(-10) |>
  arrange(no_fhs_subsistence)

farm_hh_sub_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(no_fhs_subsistence)

# 7) Farm HH (Commercial)

farm_hh_comm_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, no_fhs_commercial) |>
  top_n(10) |>
  arrange(desc(no_fhs_commercial))

farm_hh_comm_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, no_fhs_commercial) |>
  top_n(-10) |>
  arrange(no_fhs_commercial)

farm_hh_comm_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(no_fhs_commercial)

# 8) % Farm HH (Subsistence)

farm_hh_sub_percent_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_sub_percent) |>
  top_n(10) |>
  arrange(desc(hh_sub_percent))

farm_hh_sub_percent_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_sub_percent) |>
  top_n(-10) |>
  arrange(hh_sub_percent)

farm_hh_sub_percent_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(hh_sub_percent)

# 9) % Farm HH (Commercial)

farm_hh_comm_percent_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_comm_percent) |>
  top_n(10) |>
  arrange(desc(hh_comm_percent))

farm_hh_comm_percent_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_comm_percent) |>
  top_n(-10) |>
  arrange(hh_comm_percent)

farm_hh_comm_percent_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(hh_comm_percent)

# 10) Land Size/Household (Total)

land_hh_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_land_size_total) |>
  top_n(10) |>
  arrange(desc(hh_land_size_total))

land_hh_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_land_size_total) |>
  top_n(-10) |>
  arrange(hh_land_size_total)

land_hh_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(hh_land_size_total)

# 11) Land Size/Household (Subsistence)

land_hh_sub_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_land_size_sub) |>
  top_n(10) |>
  arrange(desc(hh_land_size_sub))

land_hh_sub_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_land_size_sub) |>
  top_n(-10) |>
  arrange(hh_land_size_sub)

land_hh_sub_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(hh_land_size_sub)

# 12) Land Size/Household (Commercial)

land_hh_comm_top_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_land_size_comm) |>
  top_n(10) |>
  arrange(desc(hh_land_size_comm))

land_hh_comm_bottom_10 <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, hh_land_size_comm) |>
  top_n(-10) |>
  arrange(hh_land_size_comm)

land_hh_comm_total <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(hh_land_size_comm)

######) % land in ag - get total land area

# Merge with V1_T2.4 and convert sq km to hectares #### Fix county names

total_area_sq_km <- V1_T2.4

total_area_sq_km_clean <- total_area_sq_km |>
  clean_names()

total_area_sq_km_clean_normalized <- total_area_sq_km_clean |>
  mutate(total_hectares = land_area_in_sq_km * 100,
         county = trimws(county),
         county = toupper(county),
         county = recode(county, "TANARIVER" = "TANA RIVER"),
         county = recode(county, "WESTPOKOT" = "WEST POKOT"),
         county = recode(county, "TRANSNZOIA" = "TRANS NZOIA"),
         county = recode(county, "UASINGISHU" = "UASIN GISHU"),
         county = recode(county, "HOMABAY" = "HOMA BAY"),
         county = recode(county, "NAIROBICITY" = "NAIROBI CITY")) 

# Inspect the county names that are different in each of the datasets
unique(area_ag_sub_comm_clean_normalized_county$sub_county)[which(!unique(area_ag_sub_comm_clean_normalized_county$sub_county) %in% total_area_sq_km_clean_normalized$county)]

# Merge the two datasets for ease of plotting
merged_df_ag_sub_comm_total_area <- left_join(area_ag_sub_comm_clean_normalized_county, total_area_sq_km_clean_normalized, by = c("sub_county" = "county"))

merged_df_ag_sub_comm_total_area_select <- merged_df_ag_sub_comm_total_area |>
  select(county, sub_county, land_size, land_size_subsistence, land_size_commercial, total_hectares) |>
  mutate(perc_land_farm = (land_size*100)/total_hectares)

################
# REVIEW AGAIN - KISII NOT MAKING SENSE!!!!
###############

################################################################################
# PART C: MAPS
################################################################################

# Dataset for Maps

area_ag_sub_comm_clean_normalized_county_maps <- area_ag_sub_comm_clean_normalized_county |>
  filter(sub_county != "KENYA")

# Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Plot a void map of Kenya
ggplot(kenya_counties_sf) + 
  geom_sf() + 
  theme_void()

# Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(area_ag_sub_comm_clean_normalized_county_maps$sub_county)

# Ensure that the case matches before you merge

# Inspect the county names that are different in each of the datasets
unique(area_ag_sub_comm_clean_normalized_county_maps$county)[which(!unique(area_ag_sub_comm_clean_normalized_county_maps$county) %in% kenya_counties_sf$County)]

# Rename 3 counties in the sf table
kenya_counties_sf <- kenya_counties_sf |>
  mutate(County = recode(County, "TAITA/TAVETA" = "TAITA TAVETA"),
         County = recode(County, "THARAKA-NITHI" = "THARAKA NITHI"),
         County = recode(County, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET"))

# Rename 3 counties in the livestock table
area_ag_sub_comm_clean_normalized_county_maps <- area_ag_sub_comm_clean_normalized_county_maps |>
  mutate(county = recode(county, "TAITA/TAVETA" = "TAITA TAVETA"),
         county = recode(county, "THARAKA-NITHI" = "THARAKA NITHI"),
         county = recode(county, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET"))

# Merge the two datasets for ease of plotting
merged_df_ag_sub_comm_maps <- left_join(kenya_counties_sf, area_ag_sub_comm_clean_normalized_county_maps, by = c("County" = "county"))

### Convert the m_f_ratio county names to title case
merged_df_ag_sub_comm_maps <- merged_df_ag_sub_comm_maps |> 
  mutate(County = tools::toTitleCase(tolower(County))) |>
  clean_names()


################################################################################
# Maps
################################################################################

# 1) Subsistence Land (%)

map_area_ag_sub_comm_land_sub_percent <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = land_sub_percent), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Land used for subsistence farming\n(% of Total)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 100),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_land_sub_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_land_sub_percent.png", width = 12, height = 12, dpi = 300)

# 2) Commercial Land (%)

map_area_ag_sub_comm_land_comm_percent <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = land_comm_percent), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Land used for commercial farming\n(% of Total)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 100),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_land_comm_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_land_comm_percent.png", width = 12, height = 12, dpi = 300)

# 3) Farm HH (Subsistence)

map_area_ag_sub_comm_farm_hh_sub <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = no_fhs_subsistence), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of households involved\nin subsistence farming")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 330000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_farm_hh_sub

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_farm_hh_sub.png", width = 12, height = 12, dpi = 300)

# 4) Farm HH (Commercial)

map_area_ag_sub_comm_farm_hh_comm <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = no_fhs_commercial), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of households involved\nin commercial farming")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 42000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_farm_hh_comm

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_farm_hh_comm.png", width = 12, height = 12, dpi = 300)

# 5) % Farm HH (Subsistence)

map_area_ag_sub_comm_farm_hh_sub_percent <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = hh_sub_percent), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households involved in subsistence farming\n(% of Total)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 100),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_farm_hh_sub_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_farm_hh_sub_percent.png", width = 12, height = 12, dpi = 300)

# 6) % Farm HH (Commercial)

map_area_ag_sub_comm_farm_hh_comm_percent <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = hh_comm_percent), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households involved in commercial farming\n(% of Total)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 100),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_farm_hh_comm_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_farm_hh_comm_percent.png", width = 12, height = 12, dpi = 300)


# 7) Land Size/Household (Total)

map_area_ag_sub_comm_land_hh_total <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = hh_land_size_total), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Land size (hectares) per household")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 9),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_land_hh_total

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_land_hh_total.png", width = 12, height = 12, dpi = 300)

# 8) Land Size/Household (Subsistence)

map_area_ag_sub_comm_land_hh_sub <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = hh_land_size_sub), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Land size (hectares) per household\n(Subsistence farmers)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 11),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_land_hh_sub

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_land_hh_sub.png", width = 12, height = 12, dpi = 300)

# 9) Land Size/Household (Commercial)

map_area_ag_sub_comm_land_hh_comm <- ggplot(data = merged_df_ag_sub_comm_maps)+
  geom_sf(aes(geometry = geometry, fill = hh_land_size_comm), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Land size (hectares) per household\n(Commercial farmers)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 9),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_area_ag_sub_comm_land_hh_comm

#Save the plot
ggsave("sub_pro_7_general_farming/images/area_ag_sub_comm/map_area_ag_sub_comm_land_hh_comm.png", width = 12, height = 12, dpi = 300)
