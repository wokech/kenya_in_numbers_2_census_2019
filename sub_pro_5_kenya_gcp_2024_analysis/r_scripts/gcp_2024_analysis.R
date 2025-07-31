# KNBS Gross County Product Analysis 2024
# By @kenya.in.numbers
# From GCP 2024

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

# Load the required data

# Internet Usage (2022)
mobile_phone_internet_use <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                   "datasets", "kenya_gcp_2024_tables",
                                                   "mobile_phone_internet_use.xlsx"))

# Poverty Estimates (2015 - 2022)
poverty_estimates <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                           "datasets", "kenya_gcp_2024_tables",
                                           "poverty_estimates.xlsx"))


mobile_phone_internet_use <- mobile_phone_internet_use |>
  clean_names() |>
  filter(county != "NATIONAL") 

poverty_estimates <- poverty_estimates |>
  clean_names() |>
  filter(residence_county != c("NATIONAL", "RURAL", "URBAN")) |>
  select(residence_county, x2022_percent)

merged_1 <- poverty_estimates |>
  left_join(mobile_phone_internet_use, by = c("residence_county" = "county"))

ggplot(merged_1, aes(x = used_internet_percent, y = x2022_percent)) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = residence_county))

ggplot(merged_1, aes(x = television_percent, y = x2022_percent)) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = residence_county))

ggplot(merged_1, aes(x = computer_percent, y = x2022_percent)) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = residence_county))

ggplot(merged_1, aes(x = mobile_phone_percent, y = x2022_percent)) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = residence_county))
