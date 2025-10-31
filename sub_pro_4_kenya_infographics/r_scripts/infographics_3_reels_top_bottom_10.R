# Reels for the Infographic 3 Datasets
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
                                        "datasets", "infographic_data_3.csv"))

# Remove the Kenya data
infographic_data_no_total <- infographic_data |>
  filter(County != "Kenya") 

# 3) Create top 10 and bottom 10 counties for each indicator

# a) DeskTop/Computer/Laptop/Tablet

dclt_top_10 <- infographic_data_no_total |>
  select(County, `DeskTop/Computer/Laptop/Tablet`) |>
  top_n(10) |>
  arrange(desc(`DeskTop/Computer/Laptop/Tablet`))

dclt_bottom_10 <- infographic_data_no_total |>
  select(County, `DeskTop/Computer/Laptop/Tablet`) |>
  top_n(-10) |>
  arrange(`DeskTop/Computer/Laptop/Tablet`)

dclt_total <- infographic_data |>
  filter(County == "Kenya") |>
  select(`DeskTop/Computer/Laptop/Tablet`)

# b) Bicycle

bicycle_top_10 <- infographic_data_no_total |>
  select(County, Bicycle) |>
  top_n(10) |>
  arrange(desc(Bicycle))

bicycle_bottom_10 <- infographic_data_no_total |>
  select(County, Bicycle) |>
  top_n(-10) |>
  arrange(Bicycle)

bicycle_total <- infographic_data |>
  filter(County == "Kenya") |>
  select(Bicycle)

# c) Motor Cycle

motorcycle_top_10 <- infographic_data_no_total |>
  select(County, `Motor Cycle`) |>
  top_n(10) |>
  arrange(desc(`Motor Cycle`))

motorcycle_bottom_10 <- infographic_data_no_total |>
  select(County, `Motor Cycle`) |>
  top_n(-10) |>
  arrange(`Motor Cycle`)

motorcycle_total <- infographic_data |>
  filter(County == "Kenya") |>
  select(`Motor Cycle`)

# d) Refrigerator

refrigerator_top_10 <- infographic_data_no_total |>
  select(County, Refrigerator) |>
  top_n(10) |>
  arrange(desc(Refrigerator))

refrigerator_bottom_10 <- infographic_data_no_total |>
  select(County, Refrigerator) |>
  top_n(-10) |>
  arrange(Refrigerator)

refrigerator_total <- infographic_data |>
  filter(County == "Kenya") |>
  select(Refrigerator)

# e) Truck / Lorry / Bus / Three-Wheeler Truck

tlbt_top_10 <- infographic_data_no_total |>
  select(County, `Truck/Lorry/Bus/Three Wheelertruck`) |>
  top_n(10) |>
  arrange(desc(`Truck/Lorry/Bus/Three Wheelertruck`))

tlbt_bottom_10 <- infographic_data_no_total |>
  select(County, `Truck/Lorry/Bus/Three Wheelertruck`) |>
  top_n(-10) |>
  arrange(desc(`Truck/Lorry/Bus/Three Wheelertruck`))

tlbt_total <- infographic_data |>
  filter(County == "Kenya") |>
  select(`Truck/Lorry/Bus/Three Wheelertruck`)

# f) Tuk Tuk

tuk_tuk_top_10 <- infographic_data_no_total |>
  select(County, `Tuk Tuk`) |>
  top_n(10) |>
  arrange(desc(`Tuk Tuk`))

tuk_tuk_bottom_10 <- infographic_data_no_total |>
  select(County, `Tuk Tuk`) |>
  top_n(-10) |>
  arrange(desc(`Tuk Tuk`))

tuk_tuk_total <- infographic_data |>
  filter(County == "Kenya") |>
  select(`Tuk Tuk`)

