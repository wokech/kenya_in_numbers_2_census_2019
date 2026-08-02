## Disturbed and Dangerous Counties in Kenya
## Crime and Livestock Stats for the Counties

## PASTORAL LIVESTOCK = GOATS + SHEEP + INDIGENOUS CATTLE

# 1) Load the packages required for the maps

# Solve package loading issues with options(timeout = 600) 
# increase download length time

#install.packages("sf")
library(sf) # simple features
library(tidyverse)
library(ggplot2)
library(ggrepel)
#install.packages("devtools")
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(rKenyaCensus)
library(patchwork)
library(janitor)
#install.packages("kableExtra")
library(knitr)
library(kableExtra)
library(ggthemes)
library(scales)

# 2) Map of the disturbed counties

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census, KenyaCounties_SHP

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

ggplot(kenya_counties_sf) + 
  geom_sf() + 
  theme_void()

# Dangerous and disturbed counties in Kenya

# Remove the "/"

kenya_counties_sf$County <- gsub("/", 
                                 " ", 
                                 kenya_counties_sf$County)

# select counties to highlight
highlight_counties <- c("TURKANA", "WEST POKOT", "ELGEYO MARAKWET", "BARINGO", "LAIKIPIA", "SAMBURU")

# filter the states dataset to only include the highlight states
highlighted <- kenya_counties_sf |> filter(County %in% highlight_counties)

# Highlight the required area

p0 <- ggplot() + 
  geom_sf(data = kenya_counties_sf) + 
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

p0 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_0.png", width = 12, height = 12)

p1 <- ggplot() + 
  geom_sf(data = kenya_counties_sf) + 
  geom_sf(data  = highlighted, fill = "azure4", linewidth = 0.6, color = "black") +
  theme_void() +
  theme(plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

p1

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_1.png", width = 12, height = 12)

# create a ggplot2 plot with the states and the highlighted states

p2 <- ggplot(data = highlighted) +
  geom_sf(aes(fill = County), linewidth = 0.6, show.legend = FALSE) +
  #geom_sf_label_repel(aes(label = County)) +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "",
       caption = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  theme_void()

p2

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_2.png", width = 12, height = 12)

# p1 + 
#   p2 + 
#   plot_annotation(title = "Dangerous and Disturbed",
#                   subtitle = "The six Kenyan counties that have been declared insecure because\nof banditry and cattle rustling (2023)",
#                   caption = "Source: rKenyaCensus | By: @willyokech",
#                   theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
#                                 plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
#                                 plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
#                                 plot.background = element_rect(fill = "bisque1"))) &
#   theme(text = element_text('Helvetica'))

#ggsave("images/disturbed_dangerous/county_map_1.png", width = 12, height = 8)

# 3) Generate the various dataframes required for analysis

# a) View the data available in the data catalogue

data("DataCatalogue")

# b) Load the required data

# Dangerous and disturbed
dan_dist <- c("TURKANA", "WEST POKOT", "ELGEYO MARAKWET", "BARINGO", "LAIKIPIA", "SAMBURU")

# Livestock data
df_livestock <- V4_T2.24
livestock <- df_livestock[2:393,]
livestock <- livestock |>
  clean_names()

# Remove the "/"
livestock$county <- gsub("/", " ", livestock$county)

# pastoralist livestock dataframe with new variables
livestock_select <- livestock |>
  select(county, sub_county, admin_area, farming, sheep, goats, indigenous_cattle) |>
  mutate(pasto_livestock = sheep + goats + indigenous_cattle) |>
  mutate(ind_cattle_household = round(indigenous_cattle/farming)) |>
  mutate(goats_household = round(goats/farming)) |>
  mutate(sheep_household = round(sheep/farming)) |>
  mutate(pasto_livestock_household = round(pasto_livestock/farming))

# County data for disturbed and dangerous
livestock_select_county <- livestock_select |>
  filter(admin_area == "County") |>
  filter(county %in% dan_dist)

# Subcounty data for disturbed and dangerous
livestock_select_subcounty <- livestock_select |>
  filter(admin_area == "SubCounty") |>
  filter(county %in% dan_dist)

# Area data
df_land_area <- V1_T2.7
land_area <- df_land_area[2:396,]
land_area <- land_area |>
  clean_names()

# Remove the "/"
land_area$county <- gsub("/", " ", land_area$county)
land_area$county <- gsub(" County", "", land_area$county)
land_area$county <- toupper(land_area$county)
land_area$sub_county <- toupper(land_area$sub_county)

# County area data for disturbed and dangerous
land_area_county <- land_area |>
  filter(admin_area == "County") |>
  select(county, land_area_in_sq_km) |>
  filter(county %in% dan_dist)

# Subcounty area data for disturbed and dangerous
land_area_subcounty <- land_area |>
  filter(admin_area == "SubCounty") |>
  select(county, sub_county, land_area_in_sq_km) |>
  filter(county %in% dan_dist) |>
  select(-county)

################### Final datasets used for the analysis#############

###### County data (area and livestock) for the disturbed and dangerous regions

livestock_area_county <- inner_join(livestock_select_county, land_area_county, by = "county")

livestock_area_county <- livestock_area_county |>
  mutate(ind_cattle_area = round(indigenous_cattle/land_area_in_sq_km),
         sheep_area = round(sheep/land_area_in_sq_km),
         goats_area = round(goats/land_area_in_sq_km),
         pasto_livestock_area = round(pasto_livestock/land_area_in_sq_km))

# Subcounty data (area and livestock) for the disturbed and dangerous regions

livestock_area_subcounty <- inner_join(livestock_select_subcounty, land_area_subcounty, by = "sub_county")

livestock_area_subcounty <- livestock_area_subcounty |>
  mutate(ind_cattle_area = round(indigenous_cattle/land_area_in_sq_km),
         sheep_area = round(sheep/land_area_in_sq_km),
         goats_area = round(goats/land_area_in_sq_km),
         pasto_livestock_area = round(pasto_livestock/land_area_in_sq_km))

# 4) Plots of relevant graphs (EDA)

# a) Farming Households

livestock_area_county |>
  ggplot(aes(x= reorder(county, farming), y = farming)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = 0, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = farming, label = comma(farming)), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Farming Households",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma, limits = c(0, 110000), expand = expansion(mult = c(0, 0.25))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_farming_hh.png", width = 12, height = 12)

# b) Pastoral Livestock per household

livestock_area_county |>
  ggplot(aes(x= reorder(county, pasto_livestock_household), y = pasto_livestock_household)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = pasto_livestock_household, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = 0, label = pasto_livestock_household), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Pastoral livestock per household",
       #title = "Pastoral livestock per household",
       #subtitle = "Number of goats, sheep, and indigenous cattle per household",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_hh.png", width = 12, height = 12)

# c) Animals per area

livestock_area_county |>
  ggplot(aes(x= reorder(county, pasto_livestock_area), y = pasto_livestock_area)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = 0, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = pasto_livestock_area, label = pasto_livestock_area), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = expression("Pastoral livestock density (per km"^2*")"),
       #title = "Pastoral livestock density",
       #subtitle = "The number of goats, sheep, and indigenous cattle per squared-km",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_area.png", width = 12, height = 12)

# d) Total pastoral livestock

livestock_area_county |>
  ggplot(aes(x= reorder(county, pasto_livestock), y = pasto_livestock, fill = county)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = pasto_livestock, label = county), hjust = 0, vjust = -0.5, colour = "black", size = 10) +
  geom_text(aes(y = pasto_livestock, label = comma(pasto_livestock)), hjust = 0, vjust = 1, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Pastoral Livestock",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma, limits = c(0, 3500000), expand = expansion(mult = c(0, 0.3))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_total.png", width = 12, height = 12)

# e) Total sheep

livestock_area_county |>
  ggplot(aes(x= reorder(county, sheep), y = sheep, fill = county)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = sheep, label = county), hjust = 0, vjust = -0.5, colour = "black", size = 10) +
  geom_text(aes(y = sheep, label = comma(sheep)), hjust = 0, vjust = 1, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Sheep",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma, limits = c(0, 1250000), expand = expansion(mult = c(0, 0.15))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_sheep.png", width = 12, height = 12)


# f) Total goats

livestock_area_county |>
  ggplot(aes(x= reorder(county, goats), y = goats, fill = county)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = goats, label = county), hjust = 0, vjust = -0.5, colour = "black", size = 10) +
  geom_text(aes(y = goats, label = comma(goats)), hjust = 0, vjust = 1, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Goats",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma, breaks = seq(0, 2250000, by = 1000000), limits = c(0, 2250000), expand = expansion(mult = c(0, 0.2))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_goats.png", width = 12, height = 12)

# f) Total indigenous livestock

livestock_area_county |>
  ggplot(aes(x= reorder(county, indigenous_cattle), y = indigenous_cattle, fill = county)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = indigenous_cattle, label = county), hjust = 0, vjust = -0.5, colour = "black", size = 10) +
  geom_text(aes(y = indigenous_cattle, label = comma(indigenous_cattle)), hjust = 0, vjust = 1, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Indigenous Cattle",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma, breaks = seq(0, 400000, by = 100000), limits = c(0, 400000), expand = expansion(mult = c(0, 0.35))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_ind_cattle.png", width = 12, height = 12)

# g) Sheep per household

livestock_area_county |>
  ggplot(aes(x= reorder(county, sheep_household), y = sheep_household)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = sheep_household, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = 0, label = sheep_household), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Sheep per household",
       #title = "Pastoral livestock per household",
       #subtitle = "Number of goats, sheep, and indigenous cattle per household",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_sheep_hh.png", width = 12, height = 12)


# g) Goats per household

livestock_area_county |>
  ggplot(aes(x= reorder(county, goats_household), y = goats_household)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = goats_household, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = 0, label = goats_household), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Goats per household",
       #title = "Pastoral livestock per household",
       #subtitle = "Number of goats, sheep, and indigenous cattle per household",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_goats_hh.png", width = 12, height = 12)

# h) Indigenous cattle per household

livestock_area_county |>
  ggplot(aes(x= reorder(county, ind_cattle_household), y = ind_cattle_household)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = ind_cattle_household, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = 0, label = ind_cattle_household), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = "Indigenous cattle per household",
       #title = "Pastoral livestock per household",
       #subtitle = "Number of goats, sheep, and indigenous cattle per household",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_ind_cattle_hh.png", width = 12, height = 12)

# i) Sheep per Area

livestock_area_county |>
  ggplot(aes(x= reorder(county, sheep_area), y = sheep_area)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = sheep_area, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = 0, label = sheep_area), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = expression("Sheep density (per km"^2*")"),
       #title = "Pastoral livestock per household",
       #subtitle = "Number of goats, sheep, and indigenous cattle per household",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 1.05))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_sheep_area.png", width = 12, height = 12)

# j) Goats per Area

livestock_area_county |>
  ggplot(aes(x= reorder(county, goats_area), y = goats_area)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = goats_area, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = 0, label = goats_area), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = expression("Goat density (per km"^2*")"),
       #title = "Pastoral livestock per household",
       #subtitle = "Number of goats, sheep, and indigenous cattle per household",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.55))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_goats_area.png", width = 12, height = 12)

# k) Indigenous cattle per area

livestock_area_county |>
  ggplot(aes(x= reorder(county, ind_cattle_area), y = ind_cattle_area)) + 
  geom_col(fill = "goldenrod2") + 
  geom_text(aes(y = ind_cattle_area, label = county), hjust = 0, colour = "black", size = 10) +
  geom_text(aes(y = 0, label = ind_cattle_area), hjust = 0, colour = "black", size = 10) +
  coord_flip() + 
  labs(x = "County",
       y = expression("Indigenous cattle density (per km"^2*")"),
       #title = "Pastoral livestock per household",
       #subtitle = "Number of goats, sheep, and indigenous cattle per household",
       caption = "") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.95))) +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        #plot.title = element_text(family="Helvetica", face="bold", size = 20),
        #plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        #plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_6_livestock/images/disturbed_dangerous/six_counties_pasto_livestock_ind_cattle_area.png", width = 12, height = 12)



################################################################################
# Other Analysis and SubCounty
################################################################################



# Total Numbers

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, farming), y = farming, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock), y = pasto_livestock, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock_household), y = pasto_livestock_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, sheep), y = sheep, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, goats), y = goats, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, indigenous_cattle), y = indigenous_cattle, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

# Per household

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, sheep_household), y = sheep_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, goats_household), y = goats_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, ind_cattle_household), y = ind_cattle_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

# Per area

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock_area), y = pasto_livestock_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, sheep_area), y = sheep_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, goats_area), y = goats_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty |>
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, ind_cattle_area), y = ind_cattle_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()