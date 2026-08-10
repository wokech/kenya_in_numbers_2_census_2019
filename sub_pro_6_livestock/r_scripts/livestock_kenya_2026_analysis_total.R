# Livestock Totals (Raw Numbers)
# By @kenya.in.numbers
# From the Kenya Population and Housing Census Report (2019) and rKenyaCensus

#####################
#####PART A
#####################

# Load all the required packages and libraries required for accessing the census data

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

################################################################################
# PART A: SET UP TABLES
################################################################################

# View the data available in the data catalogue

data("DataCatalogue")

# Load and save the data

df_livestock <- read.csv("sub_pro_6_livestock/datasets/df_livestock_all.csv")

# Table 1 for Kenya, County, and SubCounty Analysis
table_1_pasto <- df_livestock %>%
  clean_names()

table_1_pasto_select <- table_1_pasto %>%
  mutate(total_chicken = indigenous_chicken + exotic_chicken_layers + exotic_chicken_broilers) %>%
  mutate(total_cattle = indigenous_cattle + exotic_cattle_dairy + exotic_cattle_beef) %>%
  mutate(exotic_chicken = exotic_chicken_layers + exotic_chicken_broilers) %>%
  mutate(exotic_cattle = exotic_cattle_dairy + exotic_cattle_beef) %>%
  mutate(pastoral_livestock = indigenous_cattle + goats + sheep)

# County data
table_1_pasto_select_county <- table_1_pasto_select %>%
  filter(admin_area == "County" | sub_county == "KENYA")

# write_csv(table_1_pasto_select_county,
#           "sub_pro_6_livestock/datasets/df_livestock_analysis_table.csv")

livestock_analysis_table <- read.csv("sub_pro_6_livestock/datasets/df_livestock_analysis_table.csv")

################################################################################
# PART B: TOP AND BOTTOM 10 AND NATIONAL AVERAGE
################################################################################

# 1) Farming Households

farm_hh_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, farming) |>
  top_n(10) |>
  arrange(desc(farming))

farm_hh_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, farming) |>
  top_n(-10) |>
  arrange(farming)

farm_hh_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(farming)

# 2) Farming Households / Total Households (%)

total_households_county <- V1_T2.6 %>%
  clean_names() %>%
  filter(admin_area == "County" | sub_county == "Total") %>%
  select(sub_county, number_of_households) %>%
  mutate(sub_county = ifelse(sub_county == "Total", "KENYA", sub_county))

unique(total_households_county$sub_county)
total_households_county <- total_households_county |> 
  mutate(sub_county = tools::toTitleCase(toupper(sub_county)))
unique(total_households_county$sub_county)

merge_total_farm_hh <- merge(total_households_county, livestock_analysis_table, by = c("sub_county"))

merge_total_farm_hh_select <- merge_total_farm_hh |>
  select(sub_county, number_of_households, farming) |>
  mutate(percent_farming = round(farming*100/number_of_households))

percent_farming_top_10 <- merge_total_farm_hh_select |>
  filter(sub_county != "KENYA") |>
  select(sub_county, percent_farming) |>
  top_n(10) |>
  arrange(desc(percent_farming))

percent_farming_bottom_10 <- merge_total_farm_hh_select |>
  filter(sub_county != "KENYA") |>
  select(sub_county, percent_farming) |>
  top_n(-10) |>
  arrange(percent_farming)

percent_farming_total <- merge_total_farm_hh_select |>
  filter(sub_county == "KENYA") |>
  select(percent_farming)

# 3) Exotic Cattle (Dairy)

ex_cattle_dairy_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_cattle_dairy) |>
  top_n(10) |>
  arrange(desc(exotic_cattle_dairy))

ex_cattle_dairy_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_cattle_dairy) |>
  top_n(-10) |>
  arrange(exotic_cattle_dairy)

ex_cattle_dairy_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, exotic_cattle_dairy) |>
  select(exotic_cattle_dairy)

# 4) Exotic Cattle (Beef)

ex_cattle_beef_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_cattle_beef) |>
  top_n(10) |>
  arrange(desc(exotic_cattle_beef))

ex_cattle_beef_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_cattle_beef) |>
  top_n(-10) |>
  arrange(exotic_cattle_beef)

ex_cattle_beef_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, exotic_cattle_beef) |>
  select(exotic_cattle_beef)

# 5) Indigenous Cattle

indi_cattle_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, indigenous_cattle) |>
  top_n(10) |>
  arrange(desc(indigenous_cattle))

indi_cattle_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, indigenous_cattle) |>
  top_n(-10) |>
  arrange(indigenous_cattle)

indi_cattle_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, indigenous_cattle) |>
  select(indigenous_cattle)

# 6) Sheep

sheep_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, sheep) |>
  top_n(10) |>
  arrange(desc(sheep))

sheep_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, sheep) |>
  top_n(-10) |>
  arrange(sheep)

sheep_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, sheep) |>
  select(sheep)

# 7) Goats

goats_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, goats) |>
  top_n(10) |>
  arrange(desc(goats))

goats_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, goats) |>
  top_n(-10) |>
  arrange(goats)

goats_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, goats) |>
  select(goats)

# 8) Camels

camels_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, camels) |>
  top_n(10) |>
  arrange(desc(camels))

camels_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, camels) |>
  top_n(-10) |>
  arrange(camels)

camels_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, camels) |>
  select(camels)

# 9) Donkeys

donkeys_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, donkeys) |>
  top_n(10) |>
  arrange(desc(donkeys))

donkeys_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, donkeys) |>
  top_n(-10) |>
  arrange(donkeys)

donkeys_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, donkeys) |>
  select(donkeys)

# 10) Pigs

pigs_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, pigs) |>
  top_n(10) |>
  arrange(desc(pigs))

pigs_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, pigs) |>
  top_n(-10) |>
  arrange(pigs)

pigs_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, pigs) |>
  select(pigs)

# 11) Indigenous Chicken

indigenous_chicken_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, indigenous_chicken) |>
  top_n(10) |>
  arrange(desc(indigenous_chicken))

indigenous_chicken_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, indigenous_chicken) |>
  top_n(-10) |>
  arrange(indigenous_chicken)

indigenous_chicken_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, indigenous_chicken) |>
  select(indigenous_chicken)

# 12) Exotic Chicken (Layers)

exotic_chicken_layers_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_chicken_layers) |>
  top_n(10) |>
  arrange(desc(exotic_chicken_layers))

exotic_chicken_layers_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_chicken_layers) |>
  top_n(-10) |>
  arrange(exotic_chicken_layers)

exotic_chicken_layers_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, exotic_chicken_layers) |>
  select(exotic_chicken_layers)

# 13) Exotic Chicken (Broilers)

exotic_chicken_broilers_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_chicken_broilers) |>
  top_n(10) |>
  arrange(desc(exotic_chicken_broilers))

exotic_chicken_broilers_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_chicken_broilers) |>
  top_n(-10) |>
  arrange(exotic_chicken_broilers)

exotic_chicken_broilers_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, exotic_chicken_broilers) |>
  select(exotic_chicken_broilers)

# 14) Beehives

beehives_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, beehives) |>
  top_n(10) |>
  arrange(desc(beehives))

beehives_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, beehives) |>
  top_n(-10) |>
  arrange(beehives)

beehives_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, beehives) |>
  select(beehives)

# 15) Rabbits

rabbits_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, rabbits) |>
  top_n(10) |>
  arrange(desc(rabbits))

rabbits_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, rabbits) |>
  top_n(-10) |>
  arrange(rabbits)

rabbits_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, rabbits) |>
  select(rabbits)

# 16) Fish Ponds

fish_ponds_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, fish_ponds) |>
  top_n(10) |>
  arrange(desc(fish_ponds))

fish_ponds_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, fish_ponds) |>
  top_n(-10) |>
  arrange(fish_ponds)

fish_ponds_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, fish_ponds) |>
  select(fish_ponds)

# 17) Fish Cages

fish_cages_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, fish_cages) |>
  top_n(10) |>
  arrange(desc(fish_cages))

fish_cages_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, fish_cages) |>
  top_n(-10) |>
  arrange(fish_cages)

fish_cages_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, fish_cages) |>
  select(fish_cages)

# 18) Total Chicken

total_chicken_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, total_chicken) |>
  top_n(10) |>
  arrange(desc(total_chicken))

total_chicken_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, total_chicken) |>
  top_n(-10) |>
  arrange(total_chicken)

total_chicken_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, total_chicken) |>
  select(total_chicken)

# 19) Total Cattle

total_cattle_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, total_cattle) |>
  top_n(10) |>
  arrange(desc(total_cattle))

total_cattle_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, total_cattle) |>
  top_n(-10) |>
  arrange(total_cattle)

total_cattle_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, total_cattle) |>
  select(total_cattle)

# 20) Exotic Chicken

exotic_chicken_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_chicken) |>
  top_n(10) |>
  arrange(desc(exotic_chicken))

exotic_chicken_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_chicken) |>
  top_n(-10) |>
  arrange(exotic_chicken)

exotic_chicken_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, exotic_chicken) |>
  select(exotic_chicken)

# 21) Exotic Cattle

exotic_cattle_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_cattle) |>
  top_n(10) |>
  arrange(desc(exotic_cattle))

exotic_cattle_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, exotic_cattle) |>
  top_n(-10) |>
  arrange(exotic_cattle)

exotic_cattle_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, exotic_cattle) |>
  select(exotic_cattle)

# 22) Pastoral Livestock

pastoral_livestock_top_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, pastoral_livestock) |>
  top_n(10) |>
  arrange(desc(pastoral_livestock))

pastoral_livestock_bottom_10 <- livestock_analysis_table |>
  filter(sub_county != "KENYA") |>
  select(county, pastoral_livestock) |>
  top_n(-10) |>
  arrange(pastoral_livestock)

pastoral_livestock_total <- livestock_analysis_table |>
  filter(sub_county == "KENYA") |>
  select(county, pastoral_livestock) |>
  select(pastoral_livestock)

################################################################################
# PART C: MAPS
################################################################################

# Dataset for Maps

livestock_analysis_table_maps <- livestock_analysis_table |>
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
unique(livestock_analysis_table_maps$sub_county)

# Ensure that the case matches before you merge

# Inspect the county names that are different in each of the datasets
unique(livestock_analysis_table_maps$county)[which(!unique(livestock_analysis_table_maps$county) %in% kenya_counties_sf$County)]

# Rename 3 counties in the sf table
kenya_counties_sf <- kenya_counties_sf |>
  mutate(County = recode(County, "TAITA/TAVETA" = "TAITA TAVETA"),
         County = recode(County, "THARAKA-NITHI" = "THARAKA NITHI"),
         County = recode(County, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET"))

# Rename 3 counties in the livestock table
livestock_analysis_table_maps <- livestock_analysis_table_maps |>
  mutate(county = recode(county, "TAITA/TAVETA" = "TAITA TAVETA"),
         county = recode(county, "THARAKA-NITHI" = "THARAKA NITHI"),
         county = recode(county, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET"))

# Merge the two datasets for ease of plotting
merged_df_livestock_maps <- left_join(kenya_counties_sf, livestock_analysis_table_maps, by = c("County" = "county"))

### Convert the m_f_ratio county names to title case
merged_df_livestock_maps <- merged_df_livestock_maps |> 
  mutate(County = tools::toTitleCase(tolower(County))) |>
  clean_names()


################################################################################
# Maps
################################################################################

# 1) Farming Households Map

map_farm_hh <- ggplot(data = merged_df_livestock_maps)+
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
    limits = c(0, 350000),
    labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_farm_hh

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/farm_hh.png", width = 12, height = 12, dpi = 300)

# 2) Percentage Farming Households Map

merge_total_farm_hh_select_map <- merge_total_farm_hh_select |>
  mutate(sub_county = recode(sub_county, "TAITA/TAVETA" = "TAITA TAVETA"),
         sub_county = recode(sub_county, "THARAKA-NITHI" = "THARAKA NITHI"),
         sub_county = recode(sub_county, "ELGEYO/MARAKWET" = "ELGEYO MARAKWET")) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county))) |>
  clean_names()

# Merge Livestock and Percent Farming

merged_df_farm_hh_percent <- left_join(merged_df_livestock_maps, merge_total_farm_hh_select_map, by = c("county" = "sub_county"))

map_farm_hh_percent <- ggplot(data = merged_df_farm_hh_percent)+
  geom_sf(aes(geometry = geometry, fill = percent_farming), linewidth = 0.5)+
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

map_farm_hh_percent

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/farm_hh_percent.png", width = 12, height = 12, dpi = 300)

# 3) Exotic Cattle (Dairy)

map_exotic_cattle_dairy <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = exotic_cattle_dairy), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Exotic Cattle (Dairy)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
    limits = c(0, 150000),
    labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_exotic_cattle_dairy

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/exotic_cattle_dairy.png", width = 12, height = 12, dpi = 300)

# 4) Exotic Cattle (Beef)

map_exotic_cattle_beef <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = exotic_cattle_beef), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Exotic Cattle (Beef)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 90000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_exotic_cattle_beef

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/exotic_cattle_beef.png", width = 12, height = 12, dpi = 300)

# 5) Indigenous Cattle

map_indigenous_cattle <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = indigenous_cattle), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Indigenous Cattle")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 1450000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_indigenous_cattle

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/indigenous_cattle.png", width = 12, height = 12, dpi = 300)


# 6) Sheep

map_sheep <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = sheep), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Sheep")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 2750000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_sheep

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/sheep.png", width = 12, height = 12, dpi = 300)

# 7) Goats

map_goats <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = goats), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Goats")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 5000000),
                       labels = label_comma(),
                       breaks = c(0, 2000000, 4000000)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_goats

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/goats.png", width = 12, height = 12, dpi = 300)

# 8) Camels

map_camels <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = camels), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Camels")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 1900000),
                       labels = label_comma(), 
                       breaks = c(0, 750000, 1500000)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_camels

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/camels.png", width = 12, height = 12, dpi = 300)

# 9) Donkeys

map_donkeys <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = donkeys), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Donkeys")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 170000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_donkeys

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/donkeys.png", width = 12, height = 12, dpi = 300)

# 10) Pigs

map_pigs <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = pigs), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Pigs")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 85000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_pigs

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/pigs.png", width = 12, height = 12, dpi = 300)

# 11) Indigenous Chicken

map_indigenous_chicken <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = indigenous_chicken), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Indigenous Chicken")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 1600000),
                       labels = label_comma(),
                       breaks = c(0, 800000, 1200000, 1600000)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_indigenous_chicken

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/indigenous_chicken.png", width = 12, height = 12, dpi = 300)

# 12) Exotic Chicken (Layers)

map_exotic_chicken_layers <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = exotic_chicken_layers), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Exotic Chicken (Layers)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 1900000),
                       labels = label_comma(),
                       breaks = c(0, 750000, 1500000)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_exotic_chicken_layers

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/exotic_chicken_layers.png", width = 12, height = 12, dpi = 300)

# 13) Exotic Chicken (Broilers)

map_exotic_chicken_broilers <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = exotic_chicken_broilers), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Exotic Chicken (Broilers)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 680000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_exotic_chicken_broilers

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/exotic_chicken_broilers.png", width = 12, height = 12, dpi = 300)

# 14) Beehives

map_beehives <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = beehives), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Beehives")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 225000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_beehives

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/beehives.png", width = 12, height = 12, dpi = 300)

# 15) Rabbits

map_rabbits <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = rabbits), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Rabbits")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 60000),
                       labels = label_comma(),
                       breaks = c(0, 20000, 40000, 60000)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_rabbits

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/rabbits.png", width = 12, height = 12, dpi = 300)

# 16) Fish Ponds

map_fish_ponds <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = fish_ponds), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Fish Ponds")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 10500),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_fish_ponds

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/fish_ponds.png", width = 12, height = 12, dpi = 300)

# 17) Fish Cages

map_fish_cages <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = fish_cages), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Fish Cages")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 4000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_fish_cages

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/fish_cages.png", width = 12, height = 12, dpi = 300)

# 18) Total Chicken

map_total_chicken <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = total_chicken), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Chicken (Indigenous and Exotic)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 3700000),
                       labels = label_comma(),
                       breaks = c(0, 1500000, 3000000)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_total_chicken

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/total_chicken.png", width = 12, height = 12, dpi = 300)

# 19) Total Cattle

map_total_cattle <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = total_cattle), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Cattle (Indigenous and Exotic)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 1500000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_total_cattle

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/total_cattle.png", width = 12, height = 12, dpi = 300)

# 20) Exotic Chicken

map_exotic_chicken <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = exotic_chicken), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Exotic Chicken (Broilers and Layers)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 2600000),
                       labels = label_comma(),
                       breaks = c(0, 1000000, 2000000)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_exotic_chicken

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/exotic_chicken.png", width = 12, height = 12, dpi = 300)

# 21) Exotic Cattle

map_exotic_cattle <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = exotic_cattle), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Exotic Cattle (Dairy and Beef)")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 190000),
                       labels = label_comma()
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_exotic_cattle

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/exotic_cattle.png", width = 12, height = 12, dpi = 300)

# 22) Pastoral Livestock

map_pastoral_livestock <- ggplot(data = merged_df_livestock_maps)+
  geom_sf(aes(geometry = geometry, fill = pastoral_livestock), linewidth = 0.5)+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of Pastoral Livestock")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c("#FEFAE0", "#DDA15E", "#BC6C25", "#780000"),
                       limits = c(0, 8300000),
                       labels = label_comma(),
                       breaks = c(0, 4000000, 8000000)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_pastoral_livestock

#Save the plot
ggsave("sub_pro_6_livestock/images/livestock_kenya_2026_analysis_total/pastoral_livestock.png", width = 12, height = 12, dpi = 300)


