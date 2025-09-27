# KNBS Census (2019)
# By @kenya.in.numbers

###############################
# Pop Density,  Number of HH, and Avg HH size correlations
###############################

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

# Internet Usage (2022)

infographic_data <- read_csv(here::here("sub_pro_4_kenya_infographics", 
                                            "datasets", "infographic_data_1.csv"))

infographic_data_clean <- infographic_data |>
  clean_names() |>
  filter(county != "Kenya")

# Number of Households vs Average Household Size

# Thresholds
hh_thresh <- median(infographic_data_clean$number_of_households)
avg_hh_thresh <- median(infographic_data_clean$average_household_size)

ggplot(infographic_data_clean, aes(x = number_of_households, y = average_household_size)) +
  geom_jitter(color = "purple", size = 3, alpha = 0.75) +
  geom_text_repel(data = subset(infographic_data_clean, 
                                number_of_households > hh_thresh & average_household_size > avg_hh_thresh),
                  aes(label = county), color = "black", size = 6) +
  labs(x = "Number of Households", y = "Average Household Size", title = "") +
  annotate("rect", xmin = hh_thresh, xmax = Inf,
           ymin = avg_hh_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = avg_hh_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = hh_thresh, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_correlations/avg_hh_vs_no_hh.png",
       width = 12, height = 12, dpi = 300)

# Population Density vs Number of Households

# Thresholds
hh_thresh <- median(infographic_data_clean$number_of_households)
pop_density_thresh <- median(infographic_data_clean$population_density_no_per_sq_km)

ggplot(infographic_data_clean, aes(x = number_of_households, y = population_density_no_per_sq_km)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(data = subset(infographic_data_clean, 
                                number_of_households > hh_thresh & population_density_no_per_sq_km > pop_density_thresh),
                  aes(label = county), color = "brown4", size = 8) +
  labs(x = "Number of Households", y = "Population Density (sq. km.)", title = "") +
  annotate("rect", xmin = hh_thresh, xmax = Inf,
           ymin = pop_density_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = pop_density_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = hh_thresh, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_correlations/pop_density_vs_no_hh.png",
       width = 12, height = 12, dpi = 300)

# Population Density vs Average Household Size

# Thresholds
avg_hh_thresh <- median(infographic_data_clean$average_household_size)
pop_density_thresh <- median(infographic_data_clean$population_density_no_per_sq_km)

ggplot(infographic_data_clean, aes(x = average_household_size, y = population_density_no_per_sq_km)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(data = subset(infographic_data_clean, 
                                average_household_size > avg_hh_thresh & population_density_no_per_sq_km > pop_density_thresh),
                  aes(label = county), color = "brown4", size = 8) +
  labs(x = "Average Household Size", y = "Population Density (sq. km.)", title = "") +
  annotate("rect", xmin = avg_hh_thresh, xmax = Inf,
           ymin = pop_density_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = pop_density_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = avg_hh_thresh, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_correlations/pop_density_vs_avg_hh.png",
       width = 12, height = 12, dpi = 300)