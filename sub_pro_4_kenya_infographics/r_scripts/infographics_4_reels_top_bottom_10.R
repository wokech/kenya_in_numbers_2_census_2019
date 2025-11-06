# Reels for the Infographic 4 Datasets
# By @kenya.in.numbers

# Data: Data: Census(2019)

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
                                        "datasets", "infographic_data_4.csv"))

# Remove the Kenya data
infographic_data_no_total <- infographic_data |>
  filter(county != "Kenya") 

# 3) Create top 10 and bottom 10 counties for each indicator

# a) Piped Water

piped_water_top_10 <- infographic_data_no_total |>
  select(county, total_piped) |>
  top_n(10) |>
  arrange(desc(total_piped))

piped_water_bottom_10 <- infographic_data_no_total |>
  select(county, total_piped) |>
  top_n(-10) |>
  arrange(total_piped)

piped_water_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(total_piped)

# b) Open Bush Defecation

open_bush_top_10 <- infographic_data_no_total |>
  select(county, open_bush) |>
  top_n(10) |>
  arrange(desc(open_bush))

open_bush_bottom_10 <- infographic_data_no_total |>
  select(county, open_bush) |>
  top_n(-10) |>
  arrange(open_bush)

open_bush_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(open_bush)

# c) Flushing Toilet

flushing_toilet_top_10 <- infographic_data_no_total |>
  select(county, total_flush) |>
  top_n(10) |>
  arrange(desc(total_flush))

flushing_toilet_bottom_10 <- infographic_data_no_total |>
  select(county, total_flush) |>
  top_n(-10) |>
  arrange(total_flush)

flushing_toilet_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(total_flush)

# d) LPG Gas + Biogas for Cooking

total_gas_top_10 <- infographic_data_no_total |>
  select(county, total_gas) |>
  top_n(10) |>
  arrange(desc(total_gas))

total_gas_bottom_10 <- infographic_data_no_total |>
  select(county, total_gas) |>
  top_n(-10) |>
  arrange(total_gas)

total_gas_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(total_gas)

# e) Firewood and Charcoal for Cooking

total_wood_top_10 <- infographic_data_no_total |>
  select(county, total_wood) |>
  top_n(10) |>
  arrange(desc(total_wood))

total_wood_bottom_10 <- infographic_data_no_total |>
  select(county, total_wood) |>
  top_n(-10) |>
  arrange(total_wood)

total_wood_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(total_wood)

# f) Electricity for Lighting

elec_light_top_10 <- infographic_data_no_total |>
  select(county, mains_electricity) |>
  top_n(10) |>
  arrange(desc(mains_electricity))

elec_light_bottom_10 <- infographic_data_no_total |>
  select(county, mains_electricity) |>
  top_n(-10) |>
  arrange(mains_electricity)

elec_light_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(mains_electricity)

# g) Solar for Lighting

solar_light_top_10 <- infographic_data_no_total |>
  select(county, solar) |>
  top_n(10) |>
  arrange(desc(solar))

solar_light_bottom_10 <- infographic_data_no_total |>
  select(county, solar) |>
  top_n(-10) |>
  arrange(solar)

solar_light_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(solar)

# g) Torch for Lighting

torch_light_top_10 <- infographic_data_no_total |>
  select(county, total_torch) |>
  top_n(10) |>
  arrange(desc(total_torch))

torch_light_bottom_10 <- infographic_data_no_total |>
  select(county, total_torch) |>
  top_n(-10) |>
  arrange(total_torch)

torch_light_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(total_torch)

# h) Paraffin for Lighting

paraffin_light_top_10 <- infographic_data_no_total |>
  select(county, total_paraffin) |>
  top_n(10) |>
  arrange(desc(total_paraffin))

paraffin_light_bottom_10 <- infographic_data_no_total |>
  select(county, total_paraffin) |>
  top_n(-10) |>
  arrange(total_paraffin)

paraffin_light_total <- infographic_data |>
  filter(county == "Kenya") |>
  select(total_paraffin)

