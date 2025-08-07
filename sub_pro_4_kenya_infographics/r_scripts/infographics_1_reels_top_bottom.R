# Reels for the Infographic Datasets
# By @kenya.in.numbers

# Data: Kenya GCP (2024)

#####################
#####PART A
#####################

# 1) Load all the required packages and libraries

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

# 2) Load the infographic data
infographic_data <- read_csv(here::here("sub_pro_4_kenya_infographics", 
                                        "datasets", "infographic_data_1.csv"))

# Remove the Kenya data
infographic_data_no_total <- infographic_data |>
  filter(County != "Kenya")

# 3) Create top 10 and bottom 10 counties for each indicator

# a) M/F Ratio

m_f_ratio_top_10 <- infographic_data_no_total |>
  select(County, M_F_Ratio_per_100) |>
  top_n(10)

m_f_ratio_bottom_10 <- infographic_data_no_total |>
  select(County, M_F_Ratio_per_100) |>
  top_n(-10)

# b) Number of HouseHolds
NumberOfHouseholds


AverageHouseholdSize
Population
LandArea(in Sq. Km)
Population Density(No. per Sq. Km)
share_land_area
MPO_Total_Perc
UoI_Total_Perc
StandAloneRadio
FunctionalTV
Car