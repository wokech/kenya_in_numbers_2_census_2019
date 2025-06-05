# Are more Kenyans men or women? Human Sex Ratio in Kenya's 47 Counties
# By @willyokech
# Inspired by Rose Mintzer-Sweeney
# https://blog.datawrapper.de/gender-ratio-american-history/
# Data: rKenyaCensus
# Map generation: https://shelkariuki.netlify.app/post/firstmap/

# 1) Load all the required packages and libraries 
# required for accessing the census data

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_1 <- V1_T2.2

# 4) Create new column for male:female ratio per 100
library(tidyverse)

# Calculate the male:female ratio per 100
df_1_ratio <- df_1 %>%
  mutate(m_f_ratio = Male/Female,
         m_f_ratio_100 = round(m_f_ratio*100, 0))

# Keep the County, Total, and ratio columns

df_1_ratio_only <- df_1_ratio %>%
  select(County, m_f_ratio_100, Total)

# Remove the "Total" row
df_1_ratio_only_county <- df_1_ratio %>%
  select(County, m_f_ratio_100, Total) %>%
  filter(County != "Total")

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

### Inspect the county names in the male/female ratio dataset
df_1_ratio_only_county_unique <- unique(df_1_ratio_only_county$County)
df_1_ratio_only_county_unique

### Inspect the county names of the shape file
counties_KenyaSHP <- KenyaSHP %>% 
  st_drop_geometry() %>% 
  select(COUNTY) %>% 
  pull() %>%
  unique()

counties_KenyaSHP

### Convert the m_f_ratio county names to title case
df_1_ratio_only_county <- df_1_ratio_only_county %>% 
  ungroup() %>% 
  mutate(County = tools::toTitleCase(tolower(County)))

### Inspect the county names of the m_f_ratio data again 
df_1_ratio_only_county_unique <- unique(df_1_ratio_only_county$County)


### Inspect the county names that are different in each of the datasets
unique(df_1_ratio_only_county$County)[which(!unique(df_1_ratio_only_county$County) %in% counties_KenyaSHP)]


df_1_ratio_only_county <- df_1_ratio_only_county %>% 
  mutate(County = ifelse(County == "Taita/Taveta", "Taita Taveta",
                  ifelse(County == "Tharaka-Nithi", "Tharaka",
                  ifelse(County == "Elgeyo/Marakwet", "Keiyo-Marakwet",
                  ifelse(County == "Nairobi City", "Nairobi", County)))))

# Check again for unique datasets
unique(df_1_ratio_only_county$County)[which(!unique(df_1_ratio_only_county$County) %in% counties_KenyaSHP)]

# 7) Join the shapefile and the data

### Rename the COUNTY variable, to match the variable name in the shapefile data
df_1_ratio_only_county <- df_1_ratio_only_county %>% 
  rename(COUNTY = County)

### Ensure that there are no leading or trailing spaces in the county variable
KenyaSHP$COUNTY <- trimws(KenyaSHP$COUNTY)
df_1_ratio_only_county$COUNTY <- trimws(df_1_ratio_only_county$COUNTY)

### Merge the data
merged_df <- left_join(KenyaSHP, df_1_ratio_only_county, by = "COUNTY")

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

################
# HAD TO REMOVE THE SCALE BREAK TO ALLOW FOR PLOT/PANEL BACKGROUND TO CHANGE!
###############

library(patchwork)

barplot <- df_1_ratio_only_county %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  coord_flip() + 
  scale_fill_gradient(low = "azure3", high = "purple") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 18),
        axis.title.y =element_text(size = 18, angle = 90),
        axis.text.x =element_text(size = 14),
        axis.text.y =element_text(size = 10),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 10, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "bisque", color = "bisque"), 
        panel.background = element_rect(fill = "bisque", color = "bisque"),
        legend.position = "none") + 
    geom_hline(yintercept = 100, linetype="dashed", color = "blue", size=0.75) +
    ggtext::geom_richtext(aes(x = 15 , y = 100, 
                              label = "Male:Female ratio = 1:1"), size = 5, angle=90, vjust = 1.5) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males\nper 100 females",
       x = "County",
       y = "Number of males per 100 females") 
  

barplot 

# Save the plot
ggsave("images/county_sex_census/all_counties_barplot.png", width = 6, height = 10)

# Plot a base plot / map.

plot(KenyaSHP$geometry, lty = 5, col = "green")


#  ggplot2()

# Legend in map is silenced because the bar graph has one

map <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) +
  scale_fill_gradient(low = "azure3", high = "purple") 
  
map

# Save the plot
ggsave("images/county_sex_census/all_counties_map.png", width = 4, height = 10)

barplot + map +
  plot_annotation(title = "Human Sex Ratio in Kenya",
                  subtitle = "The number of males per 100 females in Kenya's 47 counties",
                  caption = "Data Source: rKenyaCensus | By: @willyokech",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "bisque", color = "bisque"),
                                panel.background = element_rect(fill = "bisque", color = "bisque"))) &
  theme(text = element_text('Helvetica'))

ggsave("images/county_sex_census/barplot_map.png", width = 10, height = 10, dpi = 600)

# Visualizing the human sex ratio within the different economic blocs

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

fcdc_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% fcdc) 
noreb_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% noreb)
lreb_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% lreb)
pwani_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% pwani)
sekeb_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% sekeb)
mkareb_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% mkareb)
nakeb_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% nakeb)
namet_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% namet)
major_ratio <- df_1_ratio_only_county %>%
  filter(COUNTY %in% major)

fcdc_ratio_plot <- fcdc_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "azure3") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

fcdc_ratio_plot

ggsave("images/county_sex_census/fcdc_ratio_plot.png", width = 6, height = 4)


noreb_ratio_plot <- noreb_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "chocolate4") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

noreb_ratio_plot

ggsave("images/county_sex_census/noreb_ratio_plot.png", width = 6, height = 4)


lreb_ratio_plot <- lreb_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkgoldenrod1") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

lreb_ratio_plot

ggsave("images/county_sex_census/lreb_ratio_plot.png", width = 6, height = 4)


pwani_ratio_plot <- pwani_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "deeppink") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

pwani_ratio_plot

ggsave("images/county_sex_census/pwani_ratio_plot.png", width = 6, height = 4)


sekeb_ratio_plot <- sekeb_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkseagreen") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

sekeb_ratio_plot

ggsave("images/county_sex_census/sekeb_ratio_plot.png", width = 6, height = 4)


mkareb_ratio_plot <- mkareb_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkslategrey") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

mkareb_ratio_plot

ggsave("images/county_sex_census/mkareb_ratio_plot.png", width = 6, height = 4)


nakeb_ratio_plot <- nakeb_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "aquamarine2") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

nakeb_ratio_plot

ggsave("images/county_sex_census/nakeb_ratio_plot.png", width = 6, height = 4)


namet_ratio_plot <- namet_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "coral2") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

namet_ratio_plot

ggsave("images/county_sex_census/namet_ratio_plot.png", width = 6, height = 4)


major_ratio_plot <- major_ratio %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkviolet") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

major_ratio_plot

ggsave("images/county_sex_census/major_ratio_plot.png", width = 6, height = 4)


# 10) County regional bloc maps

# Create a dataset for mapping
fcdc_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% fcdc, 1, 0))

### Merge the data
fcdc_merged_df <- left_join(KenyaSHP, fcdc_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
fcdc_merged_df <- fcdc_merged_df %>% 
  select(COUNTY, everything())

fcdc_map <- ggplot(data = fcdc_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "azure3")

fcdc_map

ggsave("images/county_sex_census/fcdc_map.png", width = 6, height = 8)



# Create a dataset for mapping
noreb_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% noreb, 1, 0))

### Merge the data
noreb_merged_df <- left_join(KenyaSHP, noreb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
noreb_merged_df <- noreb_merged_df %>% 
  select(COUNTY, everything())

noreb_map <- ggplot(data = noreb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "chocolate4")

noreb_map

ggsave("images/county_sex_census/noreb_map.png", width = 6, height = 8)



# Create a dataset for mapping
lreb_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% lreb, 1, 0))

### Merge the data
lreb_merged_df <- left_join(KenyaSHP, lreb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
lreb_merged_df <- lreb_merged_df %>% 
  select(COUNTY, everything())

lreb_map <- ggplot(data = lreb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "darkgoldenrod1")

lreb_map

ggsave("images/county_sex_census/lreb_map.png", width = 6, height = 8)


# Create a dataset for mapping
pwani_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% pwani, 1, 0))

### Merge the data
pwani_merged_df <- left_join(KenyaSHP, pwani_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
pwani_merged_df <- pwani_merged_df %>% 
  select(COUNTY, everything())

pwani_map <- ggplot(data = pwani_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "deeppink")

pwani_map

ggsave("images/county_sex_census/pwani_map.png", width = 6, height = 8)



# Create a dataset for mapping
sekeb_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% sekeb, 1, 0))

### Merge the data
sekeb_merged_df <- left_join(KenyaSHP, sekeb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
sekeb_merged_df <- sekeb_merged_df %>% 
  select(COUNTY, everything())

sekeb_map <- ggplot(data = sekeb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "darkseagreen")

sekeb_map

ggsave("images/county_sex_census/sekeb_map.png", width = 6, height = 8)



# Create a dataset for mapping
mkareb_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% mkareb, 1, 0))

### Merge the data
mkareb_merged_df <- left_join(KenyaSHP, mkareb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
mkareb_merged_df <- mkareb_merged_df %>% 
  select(COUNTY, everything())

mkareb_map <- ggplot(data = mkareb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "darkslategrey")

mkareb_map

ggsave("images/county_sex_census/mkareb_map.png", width = 6, height = 8)


# Create a dataset for mapping
nakeb_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% nakeb, 1, 0))

### Merge the data
nakeb_merged_df <- left_join(KenyaSHP, nakeb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
nakeb_merged_df <- nakeb_merged_df %>% 
  select(COUNTY, everything())

nakeb_map <- ggplot(data = nakeb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "aquamarine2")

nakeb_map

ggsave("images/county_sex_census/nakeb_map.png", width = 6, height = 8)



# Create a dataset for mapping
namet_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% namet, 1, 0))

### Merge the data
namet_merged_df <- left_join(KenyaSHP, namet_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
namet_merged_df <- namet_merged_df %>% 
  select(COUNTY, everything())

namet_map <- ggplot(data = namet_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "coral2")

namet_map

ggsave("images/county_sex_census/namet_map.png", width = 6, height = 8)


# Create a dataset for mapping
major_ratio_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% major, 1, 0))

### Merge the data
major_merged_df <- left_join(KenyaSHP, major_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
major_merged_df <- major_merged_df %>% 
  select(COUNTY, everything())

major_map <- ggplot(data = major_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "darkviolet")

major_map

ggsave("images/county_sex_census/major_map.png", width = 6, height = 8)

# 11) Filter data less than or greater than 1Mn

big <- df_1_ratio_only_county %>%
  filter(Total >= 1000000)

small <- df_1_ratio_only_county %>%
  filter(Total <= 1000000) 


big_plot <- big %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "brown3") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

big_plot

ggsave("images/county_sex_census/big_plot.png", width = 6, height = 4)


# Create a dataset for mapping
big_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% big$COUNTY, 1, 0))

### Merge the data
big_merged_df <- left_join(KenyaSHP, big_map, by = "COUNTY")

### Sort the data so that the County variable appears first
big_merged_df <- big_merged_df %>% 
  select(COUNTY, everything())

big_map <- ggplot(data = big_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "brown3")

big_map

ggsave("images/county_sex_census/big_map.png", width = 6, height = 8)


small_plot <- small %>%
  ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darksalmon") + 
  coord_flip() + 
  scale_y_break(c(7.5, 80)) + 
  theme_classic()+
  labs(x = "County", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

small_plot

ggsave("images/county_sex_census/small_plot.png", width = 6, height = 4)


# Create a dataset for mapping
small_map <- df_1_ratio_only_county %>%
  mutate(Present = ifelse(COUNTY %in% small$COUNTY, 1, 0))

### Merge the data
small_merged_df <- left_join(KenyaSHP, small_map, by = "COUNTY")

### Sort the data so that the County variable appears first
small_merged_df <- small_merged_df %>% 
  select(COUNTY, everything())

small_map <- ggplot(data = small_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_continuous(low = "white", high = "darksalmon")

small_map

ggsave("images/county_sex_census/small_map.png", width = 6, height = 8)

