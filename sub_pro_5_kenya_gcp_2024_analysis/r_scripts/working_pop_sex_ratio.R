# Working Population - Sex Ratio in Kenya's 47 Counties
# By @kenya.in.numbers
# Inspired by Rose Mintzer-Sweeney
# https://blog.datawrapper.de/gender-ratio-american-history/
# Data: Kenya GCP (2024)

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

# 3) Load the required data

# Working Population by County (2022)
working_pop_county <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                            "datasets", "kenya_gcp_2024_tables",
                                            "working_pop_county.xlsx"))

# 4) Data Cleaning

# Calculate the male:female ratio per 100
working_pop_county_ratio <- working_pop_county |>
  mutate(m_f_ratio = Male/Female,
         m_f_ratio_100 = round(m_f_ratio*100, 0))

# Select the County, Total, and ratio columns

working_pop_county_ratio_only <- working_pop_county_ratio |>
  select(County, m_f_ratio_100, Total)

# Remove the "Total" row
working_pop_county_ratio_only_county <- working_pop_county_ratio |>
  select(County, m_f_ratio_100, Total) |>
  filter(County != "TOTAL")

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
unique(working_pop_county_ratio_only_county$County)

# Rename Murang'a
working_pop_county_ratio_only_county <- working_pop_county_ratio_only_county |>
  mutate(County = recode(County, "MURANG’A" = "MURANG'A"))

# Need to change case and drop certain distinguishing features from the 
# population dataset county names

# Change the County names in the population dataset to Upper Case
working_pop_county_ratio_only_county$County <- toupper(working_pop_county_ratio_only_county$County)

# Inspect the county names that are different in each of the datasets
unique(working_pop_county_ratio_only_county$County)[which(!unique(working_pop_county_ratio_only_county$County) %in% kenya_counties_sf$County)]

# Merge the two datasets for ease of plotting
merged_df <- left_join(kenya_counties_sf, working_pop_county_ratio_only_county, by = "County")

# Fix the county names

merged_df$County <- gsub("/", " ", merged_df$County)
merged_df$County <- gsub("-", " ", merged_df$County)

### Convert the m_f_ratio county names to title case
merged_df <- merged_df |> 
  mutate(County = tools::toTitleCase(tolower(County)))

#####################
#####PART B - Total
#####################

# 1) Visualize the data

# a) Generate a barplot, a map, and a joint plot for all the data

# Bar plot

barplot <- merged_df |>
  ggplot(aes(x = reorder(County, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.75) + 
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
        legend.position = "") + 
  geom_hline(yintercept = 110, linetype="dashed", color = "black", size=1) +
  ggtext::geom_richtext(aes(x = 10 , y = 120, 
                            label = "National Ratio = 110:100"), 
                        size = 8, fill = "NA", label.color = "NA",
                        angle = 90) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Number of males\nper 100 females",
       x = "County",
       y = "Number of males per 100 females") 

barplot 

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/working_pop/total/barplot.png", width = 12, height = 12, dpi = 300)

# Map with legend

map <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = m_f_ratio_100), linewidth = 0.5)+
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
    limits = c(80, 400)
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               barheight = unit(1.5, "cm"), 
                               barwidth = unit(15, "cm")))

map

# Save the plot
ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/working_pop/total/map.png", width = 12, height = 12, dpi = 300)

barplot / map +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "azure2", color = "azure2"),
                                panel.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/working_pop/total/barplot_map.png", width = 12, height = 24, dpi = 300)
