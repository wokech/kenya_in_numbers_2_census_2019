# Kenya Instagram Hashtag Analysis 2
# By @kenya.in.numbers

#####################
#####PART A
#####################

# Load all the required packages and libraries required for accessing the census data

library(tidyverse)
library(readxl)
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
library(ggtext)


# Load the required data

# Insta Hashtags (July 2025)
kenya_insta_hashtags <- read_excel(here::here("sub_pro_4_kenya_infographics", 
                                              "datasets", "kenya_insta_hashtags_1.xlsx"))

kenya_insta_hashtags <- kenya_insta_hashtags |>
  clean_names()

# Poverty (%) and Normalized Hashtags (%)

# Thresholds
insta_thresh <- median(kenya_insta_hashtags$insta_hashtag_population_percent_july_2025)
poverty_thresh <- median(kenya_insta_hashtags$poverty_estimates_percent_2022)

ggplot(kenya_insta_hashtags, aes(x = poverty_estimates_percent_2022, y = insta_hashtag_population_percent_july_2025)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = hashtag), color = "brown4", size = 8) +
  labs(x = "Poverty Incidence (%)", y = "Hashtags/Population (%)", title = "") +
  annotate("rect", xmin = -Inf, xmax = poverty_thresh,
           ymin = insta_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = insta_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = poverty_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/poverty_hashtags.png", width = 12, height = 12, dpi = 300)


# GCP Share (%) and Normalized Hashtags (%)

# Thresholds
insta_thresh <- median(kenya_insta_hashtags$insta_hashtag_population_percent_july_2025)
gcp_thresh <- median(kenya_insta_hashtags$gcp_share_percent_2019_2023)

ggplot(kenya_insta_hashtags, aes(x = gcp_share_percent_2019_2023, y = insta_hashtag_population_percent_july_2025)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = hashtag), color = "brown4", size = 8) +
  labs(x = "GCP Share (%)", y = "Hashtags/Population (%)", title = "") +
  annotate("rect", xmin = gcp_thresh, xmax = Inf,
           ymin = insta_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = insta_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = gcp_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/gcp_hashtags.png", width = 12, height = 12, dpi = 300)

