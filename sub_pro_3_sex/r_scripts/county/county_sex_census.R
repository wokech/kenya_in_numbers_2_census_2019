# Are more Kenyans men or women? Human Sex Ratio in Kenya's 47 Counties
# By @afro_dataviz
# Inspired by Rose Mintzer-Sweeney
# https://blog.datawrapper.de/gender-ratio-american-history/
# Data: rKenyaCensus / Kenya Population and Housing Census (2019)

#####################
#####PART A
#####################

# 1) Load all the required packages and libraries required for accessing the census data

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
library(scales) # control axis/scale format
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot) # plotting theme
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

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_1 <- V1_T2.2 # Load the "Distribution of Population by Sex and County" dataset

# 4) Data Cleaning

# Calculate the male:female ratio per 100
df_1_ratio <- df_1 |>
  mutate(m_f_ratio = Male/Female,
         m_f_ratio_100 = round(m_f_ratio*100, 0))

# Select the County, Total, and ratio columns

df_1_ratio_only <- df_1_ratio |>
  select(County, m_f_ratio_100, Total)

# Remove the "Total" row
df_1_ratio_only_county <- df_1_ratio |>
  select(County, m_f_ratio_100, Total) |>
  filter(County != "Total")

# 5) Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Plot a void map of Kenya
ggplot(kenya_counties_sf) + 
  geom_sf() + 
  theme_void()

# 6) Inspect the county names in shapefile and in the population dataset 
# to see whether they match and merge the two datasets for ease of plotting

unique(kenya_counties_sf$County)
unique(df_1_ratio_only_county$County)

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
df_1_ratio_only_county$County <- toupper(df_1_ratio_only_county$County)

# Inspect the county names that are different in each of the datasets
unique(df_1_ratio_only_county$County)[which(!unique(df_1_ratio_only_county$County) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, df_1_ratio_only_county, by = "County")

# Fix the county names

merged_df$County <- gsub("/", " ", merged_df$County)
merged_df$County <- gsub("-", " ", merged_df$County)

### Convert the m_f_ratio county names to title case
merged_df <- merged_df |> 
  mutate(County = tools::toTitleCase(tolower(County)))

#####################
#####PART B
#####################


# Barplots and Maps

# Frontier Counties Development Council (FCDC) - 9 counties

# Bar plot

barplot_fcdc <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  gghighlight(County %in% fcdc) +
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 4,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-1.5, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 4) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"    # Orange-red
  )) +
  theme_classic() +
  theme(axis.title.x =element_text(size = 18),
        axis.title.y =element_text(size = 18, angle = 90),
        axis.text.x =element_text(size = 10),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 15 , y = 101, 
                            label = "National Ratio = 98:100"), size = 6, angle=90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males\nper 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_fcdc 

# Save the plot
#ggsave("sub_pro_3_sex/images/all_counties_barplot.png", width = 12, height = 12, dpi = 300)

# Map w/o legend

map_fcdc <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% fcdc) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"    # Orange-red
  ))

map_fcdc

# Save the plot
#ggsave("sub_pro_3_sex/images/all_counties_map.png", width = 12, height = 12, dpi = 300)

barplot_map_fcdc <- barplot_fcdc + map_fcdc +
  plot_annotation(title = "Human Sex Ratio in Kenya",
                  subtitle = "The number of males per 100 females in Kenya's 47 counties",
                  caption = "Data Source: rKenyaCensus | By: @kenya.in.numbers",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_fcdc

#ggsave("sub_pro_3_sex/images/barplot_map.png", width = 12, height = 12, dpi = 300)



noreb_ratio_plot <- noreb_ratio |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

noreb_ratio_plot

#ggsave("images/county_sex_census/noreb_ratio_plot.png", width = 6, height = 4)


lreb_ratio_plot <- lreb_ratio |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

lreb_ratio_plot

#ggsave("images/county_sex_census/lreb_ratio_plot.png", width = 6, height = 4)


pwani_ratio_plot <- pwani_ratio |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

pwani_ratio_plot

#ggsave("images/county_sex_census/pwani_ratio_plot.png", width = 6, height = 4)


sekeb_ratio_plot <- sekeb_ratio |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

sekeb_ratio_plot

#ggsave("images/county_sex_census/sekeb_ratio_plot.png", width = 6, height = 4)


mkareb_ratio_plot <- mkareb_ratio |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

mkareb_ratio_plot

#ggsave("images/county_sex_census/mkareb_ratio_plot.png", width = 6, height = 4)

nakeb_ratio_plot <- nakeb_ratio |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

nakeb_ratio_plot

#ggsave("images/county_sex_census/nakeb_ratio_plot.png", width = 6, height = 4)


namet_ratio_plot <- namet_ratio |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

namet_ratio_plot

#ggsave("images/county_sex_census/namet_ratio_plot.png", width = 6, height = 4)


major_ratio_plot <- major_ratio |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

major_ratio_plot

#ggsave("images/county_sex_census/major_ratio_plot.png", width = 6, height = 4)


# 10) County regional bloc maps

# Create a dataset for mapping
fcdc_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% fcdc, 1, 0))

### Merge the data
fcdc_merged_df <- left_join(KenyaSHP, fcdc_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
fcdc_merged_df <- fcdc_merged_df |> 
  select(COUNTY, everything())

fcdc_map <- ggplot(data = fcdc_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "azure3")

fcdc_map

#ggsave("images/county_sex_census/fcdc_map.png", width = 6, height = 8)



# Create a dataset for mapping
noreb_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% noreb, 1, 0))

### Merge the data
noreb_merged_df <- left_join(KenyaSHP, noreb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
noreb_merged_df <- noreb_merged_df |> 
  select(COUNTY, everything())

noreb_map <- ggplot(data = noreb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "chocolate4")

noreb_map

#ggsave("images/county_sex_census/noreb_map.png", width = 6, height = 8)



# Create a dataset for mapping
lreb_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% lreb, 1, 0))

### Merge the data
lreb_merged_df <- left_join(KenyaSHP, lreb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
lreb_merged_df <- lreb_merged_df |> 
  select(COUNTY, everything())

lreb_map <- ggplot(data = lreb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "darkgoldenrod1")

lreb_map

#ggsave("images/county_sex_census/lreb_map.png", width = 6, height = 8)


# Create a dataset for mapping
pwani_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% pwani, 1, 0))

### Merge the data
pwani_merged_df <- left_join(KenyaSHP, pwani_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
pwani_merged_df <- pwani_merged_df |> 
  select(COUNTY, everything())

pwani_map <- ggplot(data = pwani_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "deeppink")

pwani_map

#ggsave("images/county_sex_census/pwani_map.png", width = 6, height = 8)



# Create a dataset for mapping
sekeb_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% sekeb, 1, 0))

### Merge the data
sekeb_merged_df <- left_join(KenyaSHP, sekeb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
sekeb_merged_df <- sekeb_merged_df |> 
  select(COUNTY, everything())

sekeb_map <- ggplot(data = sekeb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "darkseagreen")

sekeb_map

#ggsave("images/county_sex_census/sekeb_map.png", width = 6, height = 8)



# Create a dataset for mapping
mkareb_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% mkareb, 1, 0))

### Merge the data
mkareb_merged_df <- left_join(KenyaSHP, mkareb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
mkareb_merged_df <- mkareb_merged_df |> 
  select(COUNTY, everything())

mkareb_map <- ggplot(data = mkareb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "darkslategrey")

mkareb_map

#ggsave("images/county_sex_census/mkareb_map.png", width = 6, height = 8)


# Create a dataset for mapping
nakeb_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% nakeb, 1, 0))

### Merge the data
nakeb_merged_df <- left_join(KenyaSHP, nakeb_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
nakeb_merged_df <- nakeb_merged_df |> 
  select(COUNTY, everything())

nakeb_map <- ggplot(data = nakeb_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "aquamarine2")

nakeb_map

#ggsave("images/county_sex_census/nakeb_map.png", width = 6, height = 8)



# Create a dataset for mapping
namet_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% namet, 1, 0))

### Merge the data
namet_merged_df <- left_join(KenyaSHP, namet_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
namet_merged_df <- namet_merged_df |> 
  select(COUNTY, everything())

namet_map <- ggplot(data = namet_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "coral2")

namet_map

#ggsave("images/county_sex_census/namet_map.png", width = 6, height = 8)


# Create a dataset for mapping
major_ratio_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% major, 1, 0))

### Merge the data
major_merged_df <- left_join(KenyaSHP, major_ratio_map, by = "COUNTY")

### Sort the data so that the County variable appears first
major_merged_df <- major_merged_df |> 
  select(COUNTY, everything())

major_map <- ggplot(data = major_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "darkviolet")

major_map

#ggsave("images/county_sex_census/major_map.png", width = 6, height = 8)

# 11) Filter data less than or greater than 1Mn

big <- df_1_ratio_only_county |>
  filter(Total >= 1000000)

small <- df_1_ratio_only_county |>
  filter(Total <= 1000000) 


big_plot <- big |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

big_plot

#ggsave("images/county_sex_census/big_plot.png", width = 6, height = 4)


# Create a dataset for mapping
big_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% big$COUNTY, 1, 0))

### Merge the data
big_merged_df <- left_join(KenyaSHP, big_map, by = "COUNTY")

### Sort the data so that the County variable appears first
big_merged_df <- big_merged_df |> 
  select(COUNTY, everything())

big_map <- ggplot(data = big_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "brown3")

big_map

#ggsave("images/county_sex_census/big_map.png", width = 6, height = 8)


small_plot <- small |>
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
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12)) + 
  geom_hline(yintercept = 98, linetype="dashed", color = "purple", size=0.5)

small_plot

#ggsave("images/county_sex_census/small_plot.png", width = 6, height = 4)


# Create a dataset for mapping
small_map <- df_1_ratio_only_county |>
  mutate(Present = ifelse(COUNTY %in% small$COUNTY, 1, 0))

### Merge the data
small_merged_df <- left_join(KenyaSHP, small_map, by = "COUNTY")

### Sort the data so that the County variable appears first
small_merged_df <- small_merged_df |> 
  select(COUNTY, everything())

small_map <- ggplot(data = small_merged_df)+
  geom_sf(aes(geometry = geometry, fill = Present))+
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males\nper 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.position="none",
        plot.caption = element_text(family = "Helvetica",size = 12))+
  scale_fill_continuous(low = "white", high = "darksalmon")

small_map

#ggsave("images/county_sex_census/small_map.png", width = 6, height = 8)

