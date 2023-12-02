## Open Defecation in Kenya

# 1) Load the required packages

# Solve package loading issues with options(timeout = 600) 
# increase download length time

#install.packages("sf")
library(sf) # simple features
library(sp)
library(tidyverse)
library(ggplot2)
# install.packages("ggrepel")
library(ggrepel)
#install.packages("devtools")
library(devtools)
#devtools::install_github('bbc/bbplot')
#library(bbplot)
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)
library(patchwork)
library(janitor)
#install.packages("kableExtra")
library(kableExtra)
library(tidyr)
library(readxl)
library(scales)
#install.packages("wordcloud")
library(wordcloud)
# install.packages("ggwordcloud")
library(ggwordcloud)
# install.packages("treemapify")
library(treemapify)

# 2) Map of Kenya

# The rKenyaCensus package includes a built-in county boundaries 
# dataset (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

ggplot(kenya_counties_sf) + 
  geom_sf() + 
  theme_minimal()

# Dangerous and disturbed counties in Kenya

# Remove the "/" and "-"

kenya_counties_sf$County <- gsub("/", 
                                 " ", 
                                 kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", 
                                 " ", 
                                 kenya_counties_sf$County)

# 3) Load the open defecation data and correct the names

df <- V4_T2.16
df <- df %>%
  clean_names()

df_national <- df[1:3,] %>%
  select(county, sub_county, admin_area, conventional_households, open_bush)

df_county <- df %>%
  filter(admin_area == "County") %>%
  select(county, sub_county, admin_area, conventional_households, open_bush)

df_subcounty <- df %>%
  filter(admin_area == "SubCounty") %>%
  select(county, sub_county, admin_area, conventional_households, open_bush)

df_county$county <- gsub("/", 
                                 " ", 
                         df_county$county)
df_county$county <- gsub("-", 
                                 " ", 
                         df_county$county)

# 4) Plot the relevant data

p1 <- df_county %>%
  ggplot(aes(x = reorder(county, open_bush), open_bush, fill = open_bush)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  geom_text(aes(label = paste(open_bush), hjust = -0.1), size = 7) +
  coord_flip(ylim = c(0, 75)) + 
  labs(x = "County",
       y = "Percentage of the population (%)",
       title = "2 out of 3 households in Turkana and Samburu\npractice open defecation",
       subtitle = "",
       caption = "Data Source: Kenya Census (2019) | By @afro_dataviz") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_distiller(palette = "Oranges") +
  theme_classic() +
  theme(legend.position="none",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.title.x = element_text(size = 24, vjust = 1, face = "bold"),
        axis.title.y = element_text(size = 24, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold", color = "black"),
        axis.text.y = element_text(size = 16, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, face = "bold", hjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2")) +
  geom_hline(yintercept = 7.4, linetype =1, color = 'grey5', size = .25) +
  annotate(geom="text", x = 40, y = 8.8, label = "Average (Kenya)" ,size = 8, angle = 90) +
  geom_hline(yintercept = 0.8, linetype =2, color = 'grey5', size = .25) +
  annotate(geom="text", x = 40, y = 2, label = "Average (Urban)" ,size = 8, angle = 90) +
  geom_hline(yintercept = 11.5, linetype =2, color = 'grey5', size = .25) +
  annotate(geom="text", x = 40, y = 13, label = "Average (Rural)" ,size = 8, angle = 90)

p1

ggsave("images/human_waste/open_def_bar.png", width = 12, height = 12, dpi = 72)

kenya_open_def_map <- kenya_counties_sf %>%
  left_join(df_county, by = c("County" = "county"))

p2 <- kenya_open_def_map %>%
  ggplot(aes(fill = open_bush)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "Oranges") +
  theme_void() + 
  labs(x = "",
       y = "",
       title = "Open defecation is most prevalent in\nthe Northern Counties",
       subtitle = "",
       caption = "Data Source: Kenya Census (2019) | By @afro_dataviz") +
  theme(axis.title.x = element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y = element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 24),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, face = "bold", hjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

p2

ggsave("images/human_waste/open_def_map.png", width = 12, height = 12, dpi = 72)

# Combine the plots together

# p1+p2+
#   plot_annotation(title = "Where is Open Defecation Still Widely Practiced in Kenya?",
#                   subtitle = "Percentage of households in every county practicing open defecation",
#                   caption = "Data Source: rKenyaCensus (2019) | By: @willyokech",
#                   theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
#                                 plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
#                                 plot.caption = element_text(family = "Helvetica",size = 12),
#                                 plot.background = element_rect(fill = "azure2"))) &
#   theme(text = element_text('Helvetica'))