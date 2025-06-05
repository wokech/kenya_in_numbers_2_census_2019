# Asbestos roofing in Kenya
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_roof <- V4_T2.12

View(df_roof)

# Table 1 for County and Subcounty Analysis
table_1 <- df_roof[4:395,]
View(table_1)

glimpse(table_1)

table_1 <- table_1 %>%
  clean_names()

table_1

table_1_select <- table_1 %>%
  select(c(county, sub_county, admin_area, asbestos_sheets))

View(table_1_select)
glimpse(table_1_select)


table_1_select_county <- table_1_select %>%
  filter(admin_area == "County")

table_1_select_subcounty <- table_1_select %>%
  filter(admin_area == "SubCounty")


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

### Inspect the county names in the asbestoss dataset
table_1_select_county_unique <- unique(table_1_select_county$county)
table_1_select_county_unique

### Inspect the county names of the shape file
counties_KenyaSHP <- KenyaSHP %>% 
  st_drop_geometry() %>% 
  select(COUNTY) %>% 
  pull() %>%
  unique()

counties_KenyaSHP

### Convert the table_1_select_county county names to title case
table_1_select_county <- table_1_select_county %>% 
  ungroup() %>% 
  mutate(county = tools::toTitleCase(tolower(county)))

### Inspect the county names of the asbestos data again 
table_1_select_county_unique <- unique(table_1_select_county$county)


### Inspect the county names that are different in each of the datasets
unique(table_1_select_county$county)[which(!unique(table_1_select_county$county) %in% counties_KenyaSHP)]


table_1_select_county <- table_1_select_county %>% 
  mutate(county = ifelse(county == "Taita/Taveta", "Taita Taveta",
                         ifelse(county == "Tharaka-Nithi", "Tharaka",
                                ifelse(county == "Elgeyo/Marakwet", "Keiyo-Marakwet",
                                       ifelse(county == "Nairobi City", "Nairobi", county)))))

# Check again for unique datasets
unique(table_1_select_county$county)[which(!unique(table_1_select_county$county) %in% counties_KenyaSHP)]

# 7) Join the shapefile and the data

### Rename the COUNTY variable, to match the variable name in the shapefile data
table_1_select_county <- table_1_select_county %>% 
  rename(COUNTY = county)

### Ensure that there are no leading or trailing spaces in the county variable
KenyaSHP$COUNTY <- trimws(KenyaSHP$COUNTY)
table_1_select_county$COUNTY <- trimws(table_1_select_county$COUNTY)

### Merge the data
merged_df <- left_join(KenyaSHP, table_1_select_county, by = "COUNTY")

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



# 9) Visualize the data

#install.packages("ggbreak")
library(ggbreak)

library(patchwork)

barplot <- table_1_select_county %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets, fill = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.8) + 
  coord_flip() + 
  scale_fill_gradient(low = "yellow", high = "brown") + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%)", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "Percentage (%)")+
  theme(axis.title.x =element_text(size = 14),
        axis.title.y =element_text(size = 14),
        axis.text.x =element_text(size = 12),
        axis.text.y =element_text(size = 10),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 12, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "azure", colour = "azure"),
        plot.background = element_rect(fill = "azure", colour = "azure"),
        legend.background = element_rect(fill = "azure", colour = "azure")) + 
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 20 , y = 1.4, label = "Average (Kenya)"), 
            size = 4, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 20 , y = 0.9, label = "Average (Rural)"), 
            size = 4,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 20 , y = 2.2, label = "Average (Urban)"), 
            size = 4,
            angle=90, vjust = 1.5)

barplot 

# Save the plot
ggsave("images/building_materials_kenya_asbestos_county/all_counties_asbestos_barplot.png", width = 6, height = 12)

# Plot a base plot / map.

plot(KenyaSHP$geometry, lty = 5, col = "green")


#  ggplot2()

# Legend in map is silenced because the bar graph has one

map <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = asbestos_sheets))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_gradient(low = "yellow", high = "brown")

map

# Save the plot
ggsave("images/building_materials_kenya_asbestos_county/all_counties_asbestos_map.png", width = 6, height = 8)

barplot + map +
  plot_annotation(title = "Where are you most likely to find an asbestos roof?",
                  subtitle = "Percentage (%) of households in every county with asbestos roofs",
                  caption = "Data Source: rKenyaCensus (2019) | By: @willyokech",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure"))) &
  theme(text = element_text('Helvetica'))

ggsave("images/building_materials_kenya_asbestos_county/all_counties_asbestos_barplot_map.png", width = 12, height = 10, dpi = 600)


# Visualizing asbestos ownership within the different economic blocs

fcdc <- c("Garissa", "Wajir", "Mandera", "Isiolo", "Marsabit", "Tana River", "Lamu")
noreb <- c("Uasin Gishu", "Trans Nzoia", "Nandi", "Keiyo-Marakwet", "West Pokot", "Baringo", "Samburu", "Turkana")
lreb <- c("Migori", "Nyamira", "Siaya", "Vihiga", "Bomet", "Bungoma", "Busia", "Homa Bay", "Kakamega", "Kisii", "Kisumu", "Nandi", "Trans Nzoia", "Kericho")
pwani <- c("Tana River", "Taita Taveta", "Lamu", "Kilifi", "Kwale", "Mombasa")
sekeb <- c("Kitui", "Machakos", "Makueni")
mkareb <- c("Nyeri", "Nyandarua", "Meru", "Tharaka", "Embu", "Kirinyaga", "Murang'a", "Laikipia", "Nakuru", "Kiambu")
nakeb <- c("Narok", "Kajiado")
namet <- c("Nairobi", "Kajiado", "Murang'a", "Kiambu", "Machakos")
major <- c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Uasin Gishu")

# Create new dataframes for the different economic blocs

fcdc_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% fcdc) 
noreb_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% noreb)
lreb_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% lreb)
pwani_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% pwani)
sekeb_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% sekeb)
mkareb_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% mkareb)
nakeb_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% nakeb)
namet_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% namet)
major_asbestos <- table_1_select_county %>%
  filter(COUNTY %in% major)


fcdc_asbestos_plot <- fcdc_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "azure3") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households with asbestos sheet roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 5 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

fcdc_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/fcdc_asbestos_plot.png", width = 6, height = 4)


noreb_asbestos_plot <- noreb_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "chocolate4") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households with asbestos sheet roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 5 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

noreb_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/noreb_asbestos_plot.png", width = 6, height = 4)


lreb_asbestos_plot <- lreb_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkgoldenrod1") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households with asbestos sheet roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 7.5 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 7.5 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 7.5 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

lreb_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/lreb_asbestos_plot.png", width = 6, height = 4)


pwani_asbestos_plot <- pwani_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "deeppink") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households with asbestos sheet roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 3 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

pwani_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/pwani_asbestos_plot.png", width = 6, height = 4)


sekeb_asbestos_plot <- sekeb_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkseagreen") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households with asbestos sheet roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +  
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 2 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 2 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 2 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

sekeb_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/sekeb_asbestos_plot.png", width = 6, height = 4)


mkareb_asbestos_plot <- mkareb_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkslategrey") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households with asbestos sheet roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 5 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)


mkareb_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/mkareb_asbestos_plot.png", width = 6, height = 4)


nakeb_asbestos_plot <- nakeb_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "aquamarine2") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households with asbestos sheet roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 1.5 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 1.5 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 1.5 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)


nakeb_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/nakeb_asbestos_plot.png", width = 6, height = 4)


namet_asbestos_plot <- namet_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "coral2") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households with asbestos sheet roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 3 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)


namet_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/namet_asbestos_plot.png", width = 6, height = 4)


major_asbestos_plot <- major_asbestos %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkviolet") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage(%) of households with asbestos-based roofs", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 3 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)


major_asbestos_plot

ggsave("images/building_materials_kenya_asbestos_county/major_asbestos_plot.png", width = 6, height = 4)


