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
#####PART B - High, Mid, and Low Population Areas
#####################

# 1) Generate a barplot, a map, and a joint plot for different populations sizes
# 0-750k, 750k-1M, and >1M

# High population counties (greater than 1M)

merged_df_high_pop <- merged_df |> 
  select(County, m_f_ratio_100, Total) |>
  filter(Total > 1000000)

counties_high_pop <- merged_df_high_pop$County

# Bar plot High population counties

barplot_high_pop <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = merged_df_high_pop) + 
  gghighlight(County %in% counties_high_pop, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
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
  ggtext::geom_richtext(aes(x = 5, y = 102, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_high_pop 

# Save the plot
ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/high_pop_counties_barplot.png", width = 12, height = 12, dpi = 300)

# Map High population counties

map_high_pop <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% counties_high_pop, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_high_pop

# Save the plot
ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/high_pop_counties_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map High population counties

barplot_map_high_pop <- barplot_high_pop / map_high_pop +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_high_pop

ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/high_pop_counties_barplot_map.png", width = 12, height = 24, dpi = 300)


# Mid population counties  (750k to 1M)

merged_df_mid_pop <- merged_df |> 
  select(County, m_f_ratio_100, Total) |>
  filter(Total > 750000 & Total < 1000000)

counties_mid_pop <- merged_df_mid_pop$County

# Bar plot Mid population counties

barplot_mid_pop <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = merged_df_mid_pop) + 
  gghighlight(County %in% counties_mid_pop, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
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
  ggtext::geom_richtext(aes(x = 4, y = 103, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_mid_pop 

# Save the plot
ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/mid_pop_counties_barplot.png", width = 12, height = 12, dpi = 300)

# Map Mid population counties

map_mid_pop <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% counties_mid_pop, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_mid_pop

# Save the plot
ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/mid_pop_counties_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map Mid population counties

barplot_map_mid_pop <- barplot_mid_pop / map_mid_pop +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_mid_pop

ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/mid_pop_counties_barplot_map.png", width = 12, height = 24, dpi = 300)


# Low population counties (0-750k)

merged_df_low_pop <- merged_df |> 
  select(County, m_f_ratio_100, Total) |>
  filter(Total <750000)

counties_low_pop <- merged_df_low_pop$County

# Bar plot Low population counties

barplot_low_pop <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75, 
           data = merged_df_low_pop) + 
  gghighlight(County %in% counties_low_pop, keep_scales = TRUE) + #keep_scales = TRUE is vital for not rescaling
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
  ggtext::geom_richtext(aes(x = 4, y = 103, 
                            label = "National Ratio = 98:100"), 
                        size = 10, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males per 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot_low_pop 

# Save the plot
ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/low_pop_counties_barplot.png", width = 12, height = 12, dpi = 300)

# Map Low population counties

map_low_pop <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
  gghighlight(County %in% counties_low_pop, keep_scales = TRUE) +
  geom_sf_text_repel(aes(label = County), size = 8,
                     force = 50, nudge_x = -2, seed = 10) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "Number of males per 100 females")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_text(family = "Helvetica",size = 28, hjust = 0.5),
        legend.text = element_text(family = "Helvetica",size = 24),
        legend.key.size = unit(1, "cm"),
        legend.position = "bottom",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  scale_fill_gradientn(colors = c(
    "#00BFC4",   # Teal
    "#C9E2E7",   # Light Aqua
    "#FFE3B3",   # Peach
    "#F8766D"),    # Orange-red
    limits = c(90, 120)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map_low_pop

# Save the plot
ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/low_pop_counties_map.png", width = 12, height = 12, dpi = 300)

# Bar plot and map Low population counties

barplot_map_low_pop <- barplot_low_pop / map_low_pop +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

barplot_map_low_pop

ggsave("sub_pro_3_sex/images/county/high_mid_low_pop/low_pop_counties_barplot_map.png", width = 12, height = 24, dpi = 300)


