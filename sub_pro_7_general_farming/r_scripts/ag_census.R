# Households practicing various forms of agriculture
# Agricultural Census

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

# ag_census_hh <- V4_T2.20

#write.csv(ag_census_hh, "sub_pro_7_general_farming/datasets/ag_census_hh.csv")

ag_census_hh <- read.csv("sub_pro_7_general_farming/datasets/ag_census_hh.csv")

# Clean the data
ag_census_hh_clean <- ag_census_hh %>%
  clean_names()

# Create new columns

ag_census_hh_clean_normalized <- ag_census_hh_clean |>
  mutate(farm_hh_percent = (farming*100)/total,
         crop_prod_perc  = (crop_production*100)/farming,
         livestock_prod_perc  = (livestock_production*100)/farming,
         aqua_fish_prod_perc = ((aquaculture+fishing)*100)/farming,
         irri_prod_perc = (irrigation*100)/farming)

# County data
ag_census_hh_clean_normalized_county <- ag_census_hh_clean_normalized %>%
  filter(admin_area == "County" | sub_county == "KENYA")         

################################################################################
# PART B: TOP AND BOTTOM 10 AND NATIONAL AVERAGE
################################################################################

# 1) Number of Farming Households

farm_hh_top_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, farming) |>
  top_n(10) |>
  arrange(desc(farming))

farm_hh_bottom_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, farming) |>
  top_n(-10) |>
  arrange(farming)

farm_hh_total <- ag_census_hh_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(farming)

# 2) Farming Households (%)

farm_hh_percent_top_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, farm_hh_percent) |>
  top_n(10) |>
  arrange(desc(farm_hh_percent))

farm_hh_percent_bottom_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, farm_hh_percent) |>
  top_n(-10) |>
  arrange(farm_hh_percent)

farm_hh_percent_total <- ag_census_hh_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(farm_hh_percent)

# 3) Crop Production (%)

crop_prod_hh_percent_top_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, crop_prod_perc) |>
  top_n(10) |>
  arrange(desc(crop_prod_perc))

crop_prod_hh_percent_bottom_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, crop_prod_perc) |>
  top_n(-10) |>
  arrange(crop_prod_perc)

crop_prod_hh_percent_total <- ag_census_hh_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(crop_prod_perc)

# 4) Livestock Prodution (%)

livestock_prod_hh_percent_top_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, livestock_prod_perc) |>
  top_n(10) |>
  arrange(desc(livestock_prod_perc))

livestock_prod_hh_percent_bottom_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, livestock_prod_perc) |>
  top_n(-10) |>
  arrange(livestock_prod_perc)

livestock_prod_hh_percent_total <- ag_census_hh_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(livestock_prod_perc)

# 5) Aqua/Fish Production (%)

aqua_fish_prod_hh_percent_top_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, aqua_fish_prod_perc) |>
  top_n(10) |>
  arrange(desc(aqua_fish_prod_perc))

aqua_fish_prod_hh_percent_bottom_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, aqua_fish_prod_perc) |>
  top_n(-10) |>
  arrange(aqua_fish_prod_perc)

aqua_fish_prod_hh_percent_total <- ag_census_hh_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(aqua_fish_prod_perc)

# 6) Irrigation (%)

irri_prod_hh_percent_top_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, irri_prod_perc) |>
  top_n(10) |>
  arrange(desc(irri_prod_perc))

irri_prod_hh_percent_bottom_10 <- ag_census_hh_clean_normalized_county |>
  filter(sub_county != "KENYA") |>
  select(county, irri_prod_perc) |>
  top_n(-10) |>
  arrange(irri_prod_perc)

irri_prod_hh_percent_total <- ag_census_hh_clean_normalized_county |>
  filter(sub_county == "KENYA") |>
  select(irri_prod_perc)

################################################################################
# PART C: MAPS
################################################################################

# Dataset for Maps

ag_census_hh_clean_normalized_county_maps <- ag_census_hh_clean_normalized_county |>
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
unique(ag_census_hh_clean_normalized_county_maps$sub_county)

# Ensure that the case matches before you merge

# Inspect the county names that are different in each of the datasets
unique(ag_census_hh_clean_normalized_county_maps$county)[which(!unique(ag_census_hh_clean_normalized_county_maps$county) %in% kenya_counties_sf$County)]

# Rename 3 counties in the sf table
kenya_counties_sf <- kenya_counties_sf |>
  mutate(County = recode(County, "TAITA/TAVETA" = "TAITA TAVETA"),
         County = recode(County, "THARAKA-NITHI" = "THARAKA NITHI"),
         County = recode(County, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET"))

# Rename 3 counties in the livestock table
ag_census_hh_clean_normalized_county_maps <- ag_census_hh_clean_normalized_county_maps |>
  mutate(county = recode(county, "TAITA/TAVETA" = "TAITA TAVETA"),
         county = recode(county, "THARAKA-NITHI" = "THARAKA NITHI"),
         county = recode(county, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET"))

# Merge the two datasets for ease of plotting
merged_df_ag_census_maps <- left_join(kenya_counties_sf, ag_census_hh_clean_normalized_county_maps, by = c("County" = "county"))

### Convert the m_f_ratio county names to title case
merged_df_ag_census_maps <- merged_df_ag_census_maps |> 
  mutate(County = tools::toTitleCase(tolower(County))) |>
  clean_names()


################################################################################
# Maps
################################################################################

# 1) Number of Farming Households

map_ag_census_farm_hh <- ggplot(data = merged_df_ag_census_maps)+
  geom_sf(aes(geometry = geometry, fill = farming), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Farming Households")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 340000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_ag_census_farm_hh

#Save the plot
ggsave("sub_pro_7_general_farming/images/ag_census/map_ag_census_farm_hh.png", width = 12, height = 12, dpi = 300)


# 2) Farming Households (%)

map_ag_census_farm_hh_percent <- ggplot(data = merged_df_ag_census_maps)+
  geom_sf(aes(geometry = geometry, fill = farm_hh_percent), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Farming Households (% of Total)")+
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

map_ag_census_farm_hh_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/ag_census/map_ag_census_farm_hh_percent.png", width = 12, height = 12, dpi = 300)

# 3) Crop Production (%)

map_ag_census_crop_prod_percent <- ggplot(data = merged_df_ag_census_maps)+
  geom_sf(aes(geometry = geometry, fill = crop_prod_perc), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households involved in crop production \n(% of Total)")+
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

map_ag_census_crop_prod_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/ag_census/map_ag_census_crop_prod_percent.png", width = 12, height = 12, dpi = 300)

# 4) Livestock Prodution (%)

map_ag_census_livestock_prod_percent <- ggplot(data = merged_df_ag_census_maps)+
  geom_sf(aes(geometry = geometry, fill = livestock_prod_perc), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households rearing livestock\n(% of Total)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(60, 100),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_ag_census_livestock_prod_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/ag_census/map_ag_census_livestock_prod_percent.png", width = 12, height = 12, dpi = 300)

# 5) Aqua/Fish Production (%)

map_ag_census_aqua_fish_prod_percent <- ggplot(data = merged_df_ag_census_maps)+
  geom_sf(aes(geometry = geometry, fill = aqua_fish_prod_perc), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households involved in Aquaculture and Fishing\n(% of Total)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 20),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_ag_census_aqua_fish_prod_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/ag_census/map_ag_census_aqua_fish_prod_percent.png", width = 12, height = 12, dpi = 300)


# 6) Irrigation (%)

map_ag_census_irri_prod_percent <- ggplot(data = merged_df_ag_census_maps)+
  geom_sf(aes(geometry = geometry, fill = irri_prod_perc), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Households using irrigation\n(% of Total)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),    # Orange-red
                       limits = c(0, 35),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_ag_census_irri_prod_percent

#Save the plot
ggsave("sub_pro_7_general_farming/images/ag_census/map_ag_census_irri_prod_percent.png", width = 12, height = 12, dpi = 300)
