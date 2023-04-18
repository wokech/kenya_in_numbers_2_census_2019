# What is the distribution of pastoralist livestock in Kenya
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

### To filter out the FOREST data
### filter_all(any_vars(grepl("FOREST", .))) or PARK...
###

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_livestock <- V4_T2.24

View(df_livestock)


# Table 1 for County and SubCounty Analysis
table_1_pasto <- df_livestock[2:393,]
View(table_1_pasto)

glimpse(table_1_pasto)

table_1_pasto <- table_1_pasto %>%
  clean_names()

table_1_pasto_select <- table_1_pasto %>%
  select(county, sub_county, admin_area, farming, sheep, goats, indigenous_cattle, exotic_cattle_dairy, exotic_cattle_beef) %>%
  mutate(total_pasto_livestock = sheep + goats + indigenous_cattle) %>%
  mutate(ind_cattle_farm_household = round(indigenous_cattle/farming)) %>%
  mutate(goats_farm_household = round(goats/farming)) %>%
  mutate(sheep_farm_household = round(sheep/farming)) %>%
  mutate(total_pasto_farm_household = round(total_pasto_livestock/farming)) %>%
  mutate(indi_exotic_ratio = round(indigenous_cattle/(exotic_cattle_dairy + exotic_cattle_beef),1))

View(table_1_pasto_select)
glimpse(table_1_pasto_select)

# County data
table_1_pasto_select_county <- table_1_pasto_select %>%
  filter(admin_area == "County")

#Subcounty data 
table_1_pasto_select_subcounty <- table_1_pasto_select %>%
  filter(admin_area == "SubCounty")

# Get the land area

df_land_area <- V2_T2.2b %>%
  clean_names()
df_land_area_county <- df_land_area[2:48,] %>%
  select(county, land_area_sq_km)

pasto_select_area <- inner_join(table_1_pasto_select_county, df_land_area_county, by = "county")

# Top 10 counties for sheep, goats, and indigenous cows combined

table_1_pasto_select_county_top10 <- table_1_pasto_select_county %>%
  select(county, sub_county, admin_area, total_pasto_livestock) %>%
  arrange(desc(total_pasto_livestock)) %>%
  slice(1:10)

View(table_1_pasto_select_county_top10)

# Top 10 counties for sheep

table_1_pasto_select_county_sheep_top10 <- table_1_pasto_select_county %>%
  select(county, sub_county, admin_area, sheep) %>%
  arrange(desc(sheep)) %>%
  slice(1:10)

View(table_1_pasto_select_county_sheep_top10)

# Top 10 counties for goats

table_1_pasto_select_county_goats_top10 <- table_1_pasto_select_county %>%
  select(county, sub_county, admin_area, goats) %>%
  arrange(desc(goats)) %>%
  slice(1:10)

View(table_1_pasto_select_county_goats_top10)

# Top 10 counties for indigenous cows

table_1_pasto_select_county_indi_cow_top10 <- table_1_pasto_select_county %>%
  select(county, sub_county, admin_area, indigenous_cattle) %>%
  arrange(desc(indigenous_cattle)) %>%
  slice(1:10)

View(table_1_pasto_select_county_indi_cow_top10)

# Ratio of indigenous cattle to exotic cattle

table_1_indi_exotic_ratio <- table_1_pasto_select_county %>%
  select(county, sub_county, admin_area, indigenous_cattle, exotic_cattle_dairy, exotic_cattle_beef) %>%
  mutate(indi_exotic_ratio = round(indigenous_cattle/(exotic_cattle_dairy + exotic_cattle_beef),1)) %>%
  arrange(desc(indi_exotic_ratio)) 


# 5) Load the packages required for the maps

#install.packages("sf")
library(sf) # simple features

#install.packages("tmap") #Thematic maps 
library(tmap)

#install.packages("leaflet") # Used for creating interactive maps
library(leaflet)

# Load the shapefiles that are downloaded from online source
KenyaSHP <- read_sf("kenyan-counties/County.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)


# To easily view the shapefile in RStudio View pane, you can drop the geometry column and view the rest of the data.

View(KenyaSHP %>% st_drop_geometry())

# Shapefile Data Inspection

print(KenyaSHP[5:9], n = 6)

colnames(KenyaSHP)

class(KenyaSHP)

# Look at the variable data types

glimpse(KenyaSHP)

# View the geometry column

KenyaSHP_geometry <- st_geometry(KenyaSHP)

### View one geometry entry
KenyaSHP_geometry[[1]]

# View the classes of the geometry columns

class(KenyaSHP_geometry) #sfc, the list-column with the geometries for each feature

class(KenyaSHP_geometry[[1]]) #sfg, the feature geometry of an individual simple feature


# Change the projection of the shapefiles (if necessary)

KenyaSHP <- st_transform(KenyaSHP, crs = 4326)

### Inspect the co-ordinate reference system
st_crs(KenyaSHP)


# 6) Clean the data, so that the counties match those in the shapefile

### Inspect the county names in the pasto livestock dataset
table_1_pasto_select_county_unique <- unique(table_1_pasto_select_county$county)
table_1_pasto_select_county_unique

### Inspect the county names of the shape file
counties_KenyaSHP <- KenyaSHP %>% 
  st_drop_geometry() %>% 
  select(COUNTY) %>% 
  pull() %>%
  unique()

counties_KenyaSHP

### Convert the table_1_pasto_select_county county names to title case
table_1_pasto_select_county <- table_1_pasto_select_county %>% 
  ungroup() %>% 
  mutate(County = tools::toTitleCase(tolower(county)))

### Inspect the county names of the pasto data again 
table_1_pasto_select_county_unique <- unique(table_1_pasto_select_county$county)


### Inspect the county names that are different in each of the datasets
unique(table_1_pasto_select_county$County)[which(!unique(table_1_pasto_select_county$County) %in% counties_KenyaSHP)]


table_1_pasto_select_county <- table_1_pasto_select_county %>% 
  mutate(County = ifelse(County == "Taita/Taveta", "Taita Taveta",
                         ifelse(County == "Tharaka-Nithi", "Tharaka",
                                ifelse(County == "Elgeyo/Marakwet", "Keiyo-Marakwet",
                                       ifelse(County == "Nairobi City", "Nairobi", County)))))

# Check again for unique datasets
unique(table_1_pasto_select_county$County)[which(!unique(table_1_pasto_select_county$County) %in% counties_KenyaSHP)]

# 7) Join the shapefile and the data

### Rename the COUNTY variable, to match the variable name in the shapefile data
table_1_pasto_select_county <- table_1_pasto_select_county %>% 
  rename(COUNTY = County)

### Ensure that there are no leading or trailing spaces in the county variable
KenyaSHP$COUNTY <- trimws(KenyaSHP$COUNTY)
table_1_pasto_select_county$COUNTY <- trimws(table_1_pasto_select_county$COUNTY)

### Merge the data
merged_df <- left_join(KenyaSHP, table_1_pasto_select_county, by = "COUNTY")

### Sort the data so that the County variable appears first
merged_df <- merged_df %>% 
  select(COUNTY, everything())


# 8) Inspect the merged data

# View the data
View(merged_df)
View(merged_df %>% st_drop_geometry())

### Class of the merged data
class(merged_df)

### Column names
colnames(merged_df)

# Glimpse
glimpse(merged_df)



# Visualizing the indigenous to exotic cattle ratio in Kenya

barplot_indi_ex_ratio <- table_1_pasto_select_county %>%
  ggplot(aes(x = reorder(COUNTY, indi_exotic_ratio), y = indi_exotic_ratio, fill = indi_exotic_ratio)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  coord_flip() + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_gradient(low = "darkred", high = "yellow") + 
  theme_classic() +
  labs(x = "County", 
       y = "Ratio of indigenous to exotic livestock", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "Ratio")+
  geom_hline(yintercept = 4.7, color = "black") +
  geom_text(aes(x = 20 , y = 4.7, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))  

barplot_indi_ex_ratio

# Save the plot
ggsave("images/livestock_kenya_county_pasto/all_counties_cow_ratio.png", width = 6, height = 8)

# Plot a base plot / map.

plot(KenyaSHP$geometry, lty = 5, col = "green")


#  ggplot2()

# Legend in map is silenced because the bar graph has one

map_indi_ex_ratio <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = indi_exotic_ratio))+
  theme_void()+
  labs(title = "",
       caption = "By @willyokech",
       fill = "")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_gradient(low = "darkred", high = "yellow") +
  theme(legend.position = "none")  

map_indi_ex_ratio

# Save the plot
ggsave("images/livestock_kenya_county_pasto/all_counties_cow_ratio_map.png", width = 6, height = 8)

barplot_indi_ex_ratio + map_indi_ex_ratio

ggsave("images/livestock_kenya_county_pasto/all_counties_cow_ratio_barplot_map.png", width = 10, height = 8)

