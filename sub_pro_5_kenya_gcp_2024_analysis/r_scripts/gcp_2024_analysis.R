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

