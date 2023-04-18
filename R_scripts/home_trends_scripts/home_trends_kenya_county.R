# What type of homes do Kenyans live in? Tenure, Mode of Acquisition, and House Type.
# By @willyokech
# Data: rKenyaCensus 
# Map generation: https://shelkariuki.netlify.app/post/firstmap/

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_1 <- V4_T2.10
View(df_1)

# 4) Preliminary filtering and cleanup

# Table 1 for National Analysis
table_1 <- df_1[1:3,]

# Tables for County and Subcounty Analysis

# Table 2 - County
table_2 <- df_1[4:395,]
table_2_c <- df_1[4:395,] %>%
  filter(AdminArea == "County")

# Table 3 - Subcounty
table_2_sc <- df_1[4:395,] %>%
  filter(AdminArea != "County")

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

### Inspect the county names in the homes dataset
table_2_c_unique <- unique(table_2_c$County)
table_2_c_unique

### Inspect the county names of the shape file
counties_KenyaSHP <- KenyaSHP %>% 
  st_drop_geometry() %>% 
  select(COUNTY) %>% 
  pull() %>%
  unique()

counties_KenyaSHP

### Convert the table_2_c county names to title case
table_2_c <- table_2_c %>% 
  ungroup() %>% 
  mutate(County = tools::toTitleCase(tolower(County)))

### Inspect the county names of the home data again 
table_2_c_unique <- unique(table_2_c$County)


### Inspect the county names that are different in each of the datasets
unique(table_2_c$County)[which(!unique(table_2_c$County) %in% counties_KenyaSHP)]


table_2_c <- table_2_c %>% 
  mutate(County = ifelse(County == "Taita/Taveta", "Taita Taveta",
                         ifelse(County == "Tharaka-Nithi", "Tharaka",
                                ifelse(County == "Elgeyo/Marakwet", "Keiyo-Marakwet",
                                       ifelse(County == "Nairobi City", "Nairobi", County)))))

# Check again for unique datasets
unique(table_2_c$County)[which(!unique(table_2_c$County) %in% counties_KenyaSHP)]

# 7) Join the shapefile and the data

### Rename the COUNTY variable, to match the variable name in the shapefile data
table_2_c <- table_2_c %>% 
  rename(COUNTY = County)

### Ensure that there are no leading or trailing spaces in the county variable
KenyaSHP$COUNTY <- trimws(KenyaSHP$COUNTY)
table_2_c$COUNTY <- trimws(table_2_c$COUNTY)

### Merge the data
merged_df <- left_join(KenyaSHP, table_2_c, by = "COUNTY")

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

barplot <- table_2_c %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc, fill = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  coord_flip() + 
  scale_fill_gradient(low = "darkred", high = "yellow") + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage (%) of households owning a home", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "Percentage (%)\nof households")+
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white")) + 
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 20 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 20 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 20 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

barplot 

# Save the plot
ggsave("images/county/all_counties_home_barplot.png", width = 7.5, height = 10)

# Plot a base plot / map.

plot(KenyaSHP$geometry, lty = 5, col = "green")


#  ggplot2()

# Legend in map is silenced because the bar graph has one

map <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = CH_Owned_Perc))+
  theme_void()+
  labs(title = "",
       caption = "By @willyokech",
       fill = "")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_gradient(low = "darkred", high = "yellow")

map

# Save the plot
ggsave("images/county/all_counties_home_map.png", width = 7.5, height = 10)

barplot + map

ggsave("images/county/all_counties_home_barplot_map.png", width = 10, height = 10)


# Visualizing home ownership within the different economic blocs

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

fcdc_home <- table_2_c %>%
  filter(COUNTY %in% fcdc) 
noreb_home <- table_2_c %>%
  filter(COUNTY %in% noreb)
lreb_home <- table_2_c %>%
  filter(COUNTY %in% lreb)
pwani_home <- table_2_c %>%
  filter(COUNTY %in% pwani)
sekeb_home <- table_2_c %>%
  filter(COUNTY %in% sekeb)
mkareb_home <- table_2_c %>%
  filter(COUNTY %in% mkareb)
nakeb_home <- table_2_c %>%
  filter(COUNTY %in% nakeb)
namet_home <- table_2_c %>%
  filter(COUNTY %in% namet)
major_home <- table_2_c %>%
  filter(COUNTY %in% major)


fcdc_home_plot <- fcdc_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "azure3") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 5 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

fcdc_home_plot

ggsave("images/county/fcdc_home_plot.png", width = 6, height = 4)


noreb_home_plot <- noreb_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "chocolate4") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 5 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

noreb_home_plot

ggsave("images/county/noreb_home_plot.png", width = 6, height = 4)


lreb_home_plot <- lreb_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkgoldenrod1") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 7.5 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 7.5 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 7.5 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

lreb_home_plot

ggsave("images/county/lreb_home_plot.png", width = 6, height = 4)


pwani_home_plot <- pwani_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "deeppink") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +

  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 3 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

pwani_home_plot

ggsave("images/county/pwani_home_plot.png", width = 6, height = 4)


sekeb_home_plot <- sekeb_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkseagreen") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +  
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 2 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 2 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 2 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

sekeb_home_plot

ggsave("images/county/sekeb_home_plot.png", width = 6, height = 4)


mkareb_home_plot <- mkareb_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkslategrey") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 5 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 5 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)


mkareb_home_plot

ggsave("images/county/mkareb_home_plot.png", width = 6, height = 4)


nakeb_home_plot <- nakeb_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "aquamarine2") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 1.5 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 1.5 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 1.5 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)


nakeb_home_plot

ggsave("images/county/nakeb_home_plot.png", width = 6, height = 4)


namet_home_plot <- namet_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "coral2") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 3 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)


namet_home_plot

ggsave("images/county/namet_home_plot.png", width = 6, height = 4)


major_home_plot <- major_home %>%
  ggplot(aes(x = reorder(COUNTY, CH_Owned_Perc), y = CH_Owned_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkviolet") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "County", 
       y = "", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  geom_hline(yintercept = 61.3, color = "black") +
  geom_text(aes(x = 3 , y = 61.3, label = "Average Home Ownership (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 86.6, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 86.6, label = "Average Home Ownership (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 21.3, linetype = "dashed", color = "black") +
  geom_text(aes(x = 3 , y = 21.3, label = "Average Home Ownership (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

  
major_home_plot

ggsave("images/county/major_home_plot.png", width = 6, height = 4)
