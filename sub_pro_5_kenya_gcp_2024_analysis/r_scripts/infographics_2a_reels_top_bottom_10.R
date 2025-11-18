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
library(readxl)

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

# Fix county names

working_pop_county_ratio$County <- gsub("/", " ", working_pop_county_ratio$County)
working_pop_county_ratio$County <- gsub("-", " ", working_pop_county_ratio$County)

### Convert the m_f_ratio county names to title case
working_pop_county_ratio <- working_pop_county_ratio |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Select the County, Total, and ratio columns

working_pop_county_ratio_only <- working_pop_county_ratio |>
  select(County, m_f_ratio_100, Total)

# Remove the "Total" row
working_pop_county_ratio_only_county <- working_pop_county_ratio |>
  select(County, m_f_ratio_100, Total) |>
  filter(County != "Total")

# Top and Bottom 10 for Working Population

working_pop_ratio_top_10 <- working_pop_county_ratio_only_county |>
  select(County, m_f_ratio_100) |>
  top_n(10, m_f_ratio_100) |>
  arrange(desc(m_f_ratio_100))

working_pop_ratio_bottom_10 <- working_pop_county_ratio_only_county |>
  select(County, m_f_ratio_100) |>
  top_n(-10, m_f_ratio_100) |>
  arrange(desc(m_f_ratio_100))

working_pop_ratio_total <- working_pop_county_ratio_only |>
  filter(County == "Total") |>
  select(m_f_ratio_100)
