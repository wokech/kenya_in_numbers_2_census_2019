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
#####PART B - National Government Administrative Areas
#####################

# 1) Generate a barplot, a map, and a joint plot for national government administrative regions

# Listing of regional economic blocs and visualizing of datasets by region

# Coast Region - 6 counties
coast <- c("Mombasa", "Kwale", "Kilifi", 
           "Tana River", "Lamu", "Taita Taveta")

coast_ratio <- merged_df |>
  filter(County %in% coast)

# Bar plot Coast Region - 6 counties

barplot_coast <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = coast_ratio) + 
  gghighlight(County %in% coast, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 12,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-4, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 12) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)) + # set limits to allow for scales to be maintained after highlighting
  theme_classic() +
  theme(axis.title.x =element_text(size = 36),
        axis.title.y =element_text(size = 36, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 2 , y = 105, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_coast 

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/coast_barplot.png", width = 12, height = 12, dpi = 300)

# Map Coast Region - 6 counties

map_coast <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% coast, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
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
    "#F8766D"),    # Orange-red
    limits = c(90, 120) # set limits to allow for scales to be maintained after highlighting
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_coast

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/coast_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map Coast Region - 6 counties

barplot_map_coast <- barplot_coast / map_coast +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_coast

ggsave("sub_pro_3_sex/images/county/reg_province/coast_barplot_map.png", width = 12, height = 24, dpi = 300)

# North Eastern Region - 3 counties
north_eastern <- c("Garissa", "Wajir", "Mandera")

north_eastern_ratio <- merged_df |>
  filter(County %in% north_eastern)

# Bar plot # North Eastern Region - 3 counties

barplot_north_eastern <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = north_eastern_ratio) + 
  gghighlight(County %in% north_eastern, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 12,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-4, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 12) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)) + # set limits to allow for scales to be maintained after highlighting
  theme_classic() +
  theme(axis.title.x =element_text(size = 36),
        axis.title.y =element_text(size = 36, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 2 , y = 95, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_north_eastern 

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/north_eastern_barplot.png", width = 12, height = 12, dpi = 300)

# Map # North Eastern Region - 3 counties

map_north_eastern <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% north_eastern, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
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
    "#F8766D"),    # Orange-red
    limits = c(90, 120) # set limits to allow for scales to be maintained after highlighting
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_north_eastern

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/north_eastern_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map # North Eastern Region - 3 counties

barplot_map_north_eastern <- barplot_north_eastern / map_north_eastern +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_north_eastern

ggsave("sub_pro_3_sex/images/county/reg_province/north_eastern_barplot_map.png", width = 12, height = 24, dpi = 300)


# Eastern Region - 8 counties
eastern <- c("Marsabit", "Isiolo", "Meru", "Tharaka Nithi", 
             "Embu", "Kitui", "Machakos", "Makueni")

eastern_ratio <- merged_df |>
  filter(County %in% eastern)

# Bar plot Eastern Region - 8 counties

barplot_eastern <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = eastern_ratio) + 
  gghighlight(County %in% eastern, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 12,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-4, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 12) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)) + # set limits to allow for scales to be maintained after highlighting
  theme_classic() +
  theme(axis.title.x =element_text(size = 36),
        axis.title.y =element_text(size = 36, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 3 , y = 102, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_eastern 

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/eastern_barplot.png", width = 12, height = 12, dpi = 300)

# Map Eastern Region - 8 counties

map_eastern <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% eastern, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
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
    "#F8766D"),    # Orange-red
    limits = c(90, 120) # set limits to allow for scales to be maintained after highlighting
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_eastern

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/eastern_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map Eastern Region - 8 counties

barplot_map_eastern <- barplot_eastern / map_eastern +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_eastern

ggsave("sub_pro_3_sex/images/county/reg_province/eastern_barplot_map.png", width = 12, height = 24, dpi = 300)


# Central Region - 5 counties
central <- c("Nyeri", "Kirinyaga", "Kiambu", 
             "Murang'a", "Nyandarua")

central_ratio <- merged_df |>
  filter(County %in% central)

# Bar plot # Central Region - 5 counties

barplot_central <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = central_ratio) + 
  gghighlight(County %in% central, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 12,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-4, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 12) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)) + # set limits to allow for scales to be maintained after highlighting
  theme_classic() +
  theme(axis.title.x =element_text(size = 36),
        axis.title.y =element_text(size = 36, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 2 , y = 102, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_central 

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/central_barplot.png", width = 12, height = 12, dpi = 300)

# Map # Central Region - 5 counties

map_central <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% central, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
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
    "#F8766D"),    # Orange-red
    limits = c(90, 120) # set limits to allow for scales to be maintained after highlighting
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_central

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/central_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map # Central Region - 5 counties

barplot_map_central <- barplot_central / map_central +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_central

ggsave("sub_pro_3_sex/images/county/reg_province/central_barplot_map.png", width = 12, height = 24, dpi = 300)


# Rift Valley Region - 14 counties
rift_valley <- c("Turkana", "West Pokot", "Samburu", 
                 "Trans Nzoia", "Uasin Gishu", "Elgeyo Marakwet", 
                 "Nandi", "Baringo", "Laikipia", "Nakuru",
                 "Narok", "Kajiado", "Kericho", "Bomet")

rift_valley_ratio <- merged_df |>
  filter(County %in% rift_valley)

# Bar plot # Rift Valley Region - 14 counties

barplot_rift_valley <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = rift_valley_ratio) + 
  gghighlight(County %in% rift_valley, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 12,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-4, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 12) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)) + # set limits to allow for scales to be maintained after highlighting
  theme_classic() +
  theme(axis.title.x =element_text(size = 36),
        axis.title.y =element_text(size = 36, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 4 , y = 102, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_rift_valley 

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/rift_valley_barplot.png", width = 12, height = 12, dpi = 300)

# Map # Rift Valley Region - 14 counties

map_rift_valley <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% rift_valley, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 100, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
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
    "#F8766D"),    # Orange-red
    limits = c(90, 120) # set limits to allow for scales to be maintained after highlighting
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_rift_valley

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/rift_valley_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map # Rift Valley Region - 14 counties

barplot_map_rift_valley <- barplot_rift_valley / map_rift_valley +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_rift_valley

ggsave("sub_pro_3_sex/images/county/reg_province/rift_valley_barplot_map.png", width = 12, height = 24, dpi = 300)


# Western Region - 4 counties
western <- c("Kakamega", "Vihiga", "Bungoma", "Busia")

western_ratio <- merged_df |>
  filter(County %in% western)

# Bar plot # Western Region - 4 counties

barplot_western <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = western_ratio) + 
  gghighlight(County %in% western, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 12,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-4, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 12) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)) + # set limits to allow for scales to be maintained after highlighting
  theme_classic() +
  theme(axis.title.x =element_text(size = 36),
        axis.title.y =element_text(size = 36, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 2 , y = 102, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_western 

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/western_barplot.png", width = 12, height = 12, dpi = 300)

# Map # Western Region - 4 counties

map_western <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% western, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
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
    "#F8766D"),    # Orange-red
    limits = c(90, 120) # set limits to allow for scales to be maintained after highlighting
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_western

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/western_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map # Western Region - 4 counties

barplot_map_western <- barplot_western / map_western +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_western

ggsave("sub_pro_3_sex/images/county/reg_province/western_barplot_map.png", width = 12, height = 24, dpi = 300)

# Nyanza Region - 6 counties
nyanza <- c("Siaya", "Homa Bay", "Kisumu", 
            "Kisii", "Nyamira", "Migori")

nyanza_ratio <- merged_df |>
  filter(County %in% nyanza)

# Bar plot # Nyanza Region - 6 counties

barplot_nyanza <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = nyanza_ratio) + 
  gghighlight(County %in% nyanza, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 12,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-4, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 12) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)) + # set limits to allow for scales to be maintained after highlighting
  theme_classic() +
  theme(axis.title.x =element_text(size = 36),
        axis.title.y =element_text(size = 36, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 3 , y = 102, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_nyanza 

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/nyanza_barplot.png", width = 12, height = 12, dpi = 300)

# Map # Nyanza Region - 6 counties

map_nyanza <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% nyanza, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
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
    "#F8766D"),    # Orange-red
    limits = c(90, 120) # set limits to allow for scales to be maintained after highlighting
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_nyanza

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/nyanza_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map # Nyanza Region - 6 counties

barplot_map_nyanza <- barplot_nyanza / map_nyanza +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_nyanza

ggsave("sub_pro_3_sex/images/county/reg_province/nyanza_barplot_map.png", width = 12, height = 24, dpi = 300)


# Nairobi Region - 1 county
nairobi <- c("Nairobi City")

nairobi_ratio <- merged_df |>
  filter(County %in% nairobi)

# Bar plot # Nairobi Region - 1 county

barplot_nairobi <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = nairobi_ratio) + 
  gghighlight(County %in% nairobi, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
  geom_text(aes(x = County, y = 0, label = County), 
            color = "black", 
            fontface = "bold",
            size = 12,
            hjust = 0) +
  geom_text(aes(x = County, y = m_f_ratio_100-4, label = m_f_ratio_100), 
            color = "black", 
            fontface = "bold",
            size = 12) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)) + # set limits to allow for scales to be maintained after highlighting
  theme_classic() +
  theme(axis.title.x =element_text(size = 36),
        axis.title.y =element_text(size = 36, angle = 90),
        axis.text.x =element_text(size = 28),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 10, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "none") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 1 , y = 102, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_nairobi 

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/nairobi_barplot.png", width = 12, height = 12, dpi = 300)

# Map # Nairobi Region - 1 county

map_nairobi <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% nairobi, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
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
    "#F8766D"),    # Orange-red
    limits = c(90, 120) # set limits to allow for scales to be maintained after highlighting
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_nairobi

# Save the plot
ggsave("sub_pro_3_sex/images/county/reg_province/nairobi_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map # Nairobi Region - 1 county

barplot_map_nairobi <- barplot_nairobi / map_nairobi +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_nairobi

ggsave("sub_pro_3_sex/images/county/reg_province/nairobi_barplot_map.png", width = 12, height = 24, dpi = 300)
