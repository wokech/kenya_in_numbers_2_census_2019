## Disturbed and Dangerous Counties in Kenya
## Crime and Livestock Stats for the Counties

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
highlighted <- kenya_counties_sf %>% filter(County %in% highlight_counties)

# Highlight the required area

p1 <- ggplot() + 
  geom_sf(data = kenya_counties_sf) + 
  geom_sf(data  = highlighted, fill = "azure4", linewidth = 0.6, color = "black") +
  theme_void() 

# create a ggplot2 plot with the states and the highlighted states
p2 <- ggplot(data = highlighted) +
  geom_sf(aes(fill = County), linewidth = 0.6, show.legend = FALSE) +
  geom_sf_label_repel(aes(label = County)) +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "",
       caption = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12)) +
  theme_void() 

p1 + 
  p2 + 
  plot_annotation(title = "Dangerous and Disturbed",
                  subtitle = "The six Kenyan counties that have been declared insecure because\nof banditry and cattle rustling (2023)",
                  caption = "Source: rKenyaCensus | By: @willyokech",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
                                plot.background = element_rect(fill = "bisque1"))) &
  theme(text = element_text('Helvetica'))

ggsave("images/disturbed_dangerous/county_map_1.png", width = 12, height = 8)

# 3) Generate the various dataframes required for analysis

# a) View the data available in the data catalogue

data("DataCatalogue")

# b) Load the required data

# Dangerous and disturbed
dan_dist <- c("TURKANA", "WEST POKOT", "ELGEYO MARAKWET", "BARINGO", "LAIKIPIA", "SAMBURU")

# Livestock data
df_livestock <- V4_T2.24
livestock <- df_livestock[2:393,]
livestock <- livestock %>%
  clean_names()

# Remove the "/"
livestock$county <- gsub("/", " ", livestock$county)

# pastoralist livestock dataframe with new variables
livestock_select <- livestock %>%
  select(county, sub_county, admin_area, farming, sheep, goats, indigenous_cattle) %>%
  mutate(pasto_livestock = sheep + goats + indigenous_cattle) %>%
  mutate(ind_cattle_household = round(indigenous_cattle/farming)) %>%
  mutate(goats_household = round(goats/farming)) %>%
  mutate(sheep_household = round(sheep/farming)) %>%
  mutate(pasto_livestock_household = round(pasto_livestock/farming))

# County data for disturbed and dangerous
livestock_select_county <- livestock_select %>%
  filter(admin_area == "County") %>%
  filter(county %in% dan_dist)

# Subcounty data for disturbed and dangerous
livestock_select_subcounty <- livestock_select %>%
  filter(admin_area == "SubCounty") %>%
  filter(county %in% dan_dist)

# Area data
df_land_area <- V1_T2.7
land_area <- df_land_area[2:396,]
land_area <- land_area %>%
  clean_names()

# Remove the "/"
land_area$county <- gsub("/", " ", land_area$county)
land_area$county <- gsub(" County", "", land_area$county)
land_area$county <- toupper(land_area$county)
land_area$sub_county <- toupper(land_area$sub_county)


# County area data for disturbed and dangerous
land_area_county <- land_area %>%
  filter(admin_area == "County") %>%
  select(county, land_area_in_sq_km) %>%
  filter(county %in% dan_dist)

# Subcounty area data for disturbed and dangerous
land_area_subcounty <- land_area %>%
  filter(admin_area == "SubCounty") %>%
  select(county, sub_county, land_area_in_sq_km) %>%
  filter(county %in% dan_dist) %>%
  select(-county)

################### Final datasets used for the analysis#############

###### County data (area and livestock) for the disturbed and dangerous regions

livestock_area_county <- inner_join(livestock_select_county, land_area_county, by = "county")

livestock_area_county <- livestock_area_county %>%
  mutate(ind_cattle_area = round(indigenous_cattle/land_area_in_sq_km),
         sheep_area = round(sheep/land_area_in_sq_km),
         goats_area = round(goats/land_area_in_sq_km),
         pasto_livestock_area = round(pasto_livestock/land_area_in_sq_km))

# Subcounty data (area and livestock) for the disturbed and dangerous regions

livestock_area_subcounty <- inner_join(livestock_select_subcounty, land_area_subcounty, by = "sub_county")

livestock_area_subcounty <- livestock_area_subcounty %>%
  mutate(ind_cattle_area = round(indigenous_cattle/land_area_in_sq_km),
         sheep_area = round(sheep/land_area_in_sq_km),
         goats_area = round(goats/land_area_in_sq_km),
         pasto_livestock_area = round(pasto_livestock/land_area_in_sq_km))

#4) Plots of relevant graphs (EDA)
# a) Farming Households

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, farming), y = farming, fill = county)) + 
  scale_fill_grey() +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Farming Households",
       title = "Farming Households",
       subtitle = "Number of households engaged in various farming activities",
       caption = "Source: rKenyaCensus | By: @willyokech") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("images/disturbed_dangerous/county_farm_house_1.png", width = 12, height = 8)

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, farming), y = farming, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

# b) Pastoral Livestock
livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, pasto_livestock), y = pasto_livestock, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock), y = pasto_livestock, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

# c) Pastoral Livestock per household

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, pasto_livestock_household), y = pasto_livestock_household, fill = county)) + 
  scale_fill_grey() +
  coord_flip() + 
  labs(x = "County",
       y = "Number of pastoral livestock per household",
       title = "Pastoral livestock per household",
       subtitle = "Number of goats, sheep, and indigenous cattle per household",
       caption = "Source: rKenyaCensus | By: @willyokech") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("images/disturbed_dangerous/county_pasto_livestock_1.png", width = 12, height = 8)

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock_household), y = pasto_livestock_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

# d) Each animal

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, sheep), y = sheep, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, sheep), y = sheep, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, goats), y = goats, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, goats), y = goats, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, indigenous_cattle), y = indigenous_cattle, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, indigenous_cattle), y = indigenous_cattle, fill = county)) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_minimal()

# e) Each animal per household

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, sheep_household), y = sheep_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, sheep_household), y = sheep_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, goats_household), y = goats_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, goats_household), y = goats_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, ind_cattle_household), y = ind_cattle_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, ind_cattle_household), y = ind_cattle_household, fill = county)) + 
  coord_flip() + 
  theme_minimal()

# Review this section to see how to style and save the image
# Require the webshot::install_phantomjs() to install package
# also include the magick package

livestock_area_county %>%
  select(county, land_area_in_sq_km) %>%
  mutate(county = str_to_title(county)) %>%
  arrange(desc(land_area_in_sq_km)) %>%
  adorn_totals("row") %>%
  rename("County" = "county",
         "Land Area (sq. km)" = "land_area_in_sq_km") %>%
  kbl(align = "c") %>%
  kable_classic() %>% 
  row_spec(row = 0, font_size = 28, color = "white", background = "#000000") %>%
  row_spec(row = c(1:7), font_size = 20) %>%
  row_spec(row = 6, extra_css = "border-bottom: 1px solid;") %>%
  row_spec(row = 7, bold = T) %>%
  save_kable(file = "images/disturbed_dangerous/area_table.png",
             zoom = 5)


# e) Animals per area

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, pasto_livestock_area), y = pasto_livestock_area, fill = county)) + 
  coord_flip() + 
  theme_minimal() +
  scale_fill_grey() +
  coord_flip() + 
  labs(x = "County",
       y = "Number of pastoral livestock per squared-km",
       title = "Pastoral livestock density",
       subtitle = "The number of goats, sheep, and indigenous cattle per squared-km",
       caption = "Source: rKenyaCensus | By: @willyokech") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("images/disturbed_dangerous/county_pasto_livestock_density_1.png", width = 12, height = 8)


livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock_area), y = pasto_livestock_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, sheep_area), y = sheep_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, sheep_area), y = sheep_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, goats_area), y = goats_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, goats_area), y = goats_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, ind_cattle_area), y = ind_cattle_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, ind_cattle_area), y = ind_cattle_area, fill = county)) + 
  coord_flip() + 
  theme_minimal()

