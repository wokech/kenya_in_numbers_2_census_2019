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
  top_n(10) |>
  arrange(desc(M_F_Ratio_per_100))

m_f_ratio_bottom_10 <- infographic_data_no_total |>
  select(County, M_F_Ratio_per_100) |>
  top_n(-10) |>
  arrange(desc(M_F_Ratio_per_100))

m_f_ratio_total <- infographic_data |>
  filter(County == "Kenya") |>
  select(M_F_Ratio_per_100)

# b) Number of HouseHolds

number_hh_top_10 <- infographic_data_no_total |>
  select(County, NumberOfHouseholds) |>
  top_n(10) |>
  arrange(desc(NumberOfHouseholds))

number_hh_bottom_10 <- infographic_data_no_total |>
  select(County, NumberOfHouseholds) |>
  top_n(-10) |>
  arrange(desc(NumberOfHouseholds))

number_hh_total <- infographic_data |>
  filter(County == "Kenya") |>
select(NumberOfHouseholds)

# c) Average Household Size

avg_hh_size_top_10 <- infographic_data_no_total |>
  select(County, AverageHouseholdSize) |>
  top_n(10) |>
  arrange(desc(AverageHouseholdSize))

avg_hh_size_bottom_10 <- infographic_data_no_total |>
  select(County, AverageHouseholdSize) |>
  top_n(-10) |>
  arrange(desc(AverageHouseholdSize))

avg_hh_size_total <- infographic_data |>
  filter(County == "Kenya") |>
select(AverageHouseholdSize)

# d) Population

population_top_10 <- infographic_data_no_total |>
  select(County, Population) |>
  top_n(10) |>
  arrange(desc(Population))

population_bottom_10 <- infographic_data_no_total |>
  select(County, Population) |>
  top_n(-10) |>
  arrange(desc(Population))

population_total <- infographic_data |>
  filter(County == "Kenya") |>
select(Population)

# e) Land Area

land_area_top_10 <- infographic_data_no_total |>
  select(County, `LandArea(in Sq. Km)`) |>
  top_n(10) |>
  arrange(desc(`LandArea(in Sq. Km)`))

land_area_bottom_10 <- infographic_data_no_total |>
  select(County, `LandArea(in Sq. Km)`) |>
  top_n(-10) |>
  arrange(desc(`LandArea(in Sq. Km)`))

land_area_total <- infographic_data |>
  filter(County == "Kenya") |>
select(`LandArea(in Sq. Km)`)

# f) Population Density

pop_density_top_10 <- infographic_data_no_total |>
  select(County, `Population Density(No. per Sq. Km)`) |>
  top_n(10) |>
  arrange(desc(`Population Density(No. per Sq. Km)`))

pop_density_bottom_10 <- infographic_data_no_total |>
  select(County, `Population Density(No. per Sq. Km)`) |>
  top_n(-10) |>
  arrange(desc(`Population Density(No. per Sq. Km)`))

pop_density_total <- infographic_data |>
  filter(County == "Kenya") |>
select(`Population Density(No. per Sq. Km)`)

# f) Share Land Area

share_land_area_top_10 <- infographic_data_no_total |>
  select(County, share_land_area) |>
  top_n(10) |>
  arrange(desc(share_land_area))

share_land_area_bottom_10 <- infographic_data_no_total |>
  select(County, share_land_area) |>
  top_n(-10) |>
  arrange(desc(share_land_area))

share_land_area_total <- infographic_data |>
  filter(County == "Kenya") |>
select(share_land_area)

# g) Mobile Phone Ownership (%)

mpo_top_10 <- infographic_data_no_total |>
  select(County, MPO_Total_Perc) |>
  top_n(10) |>
  arrange(desc(MPO_Total_Perc))

mpo_bottom_10 <- infographic_data_no_total |>
  select(County, MPO_Total_Perc) |>
  top_n(-10) |>
  arrange(desc(MPO_Total_Perc))

mpo_total <- infographic_data |>
  filter(County == "Kenya") |>
select(MPO_Total_Perc)

# g) Use of Internet (%)

uoi_top_10 <- infographic_data_no_total |>
  select(County, UoI_Total_Perc) |>
  top_n(10) |>
  arrange(desc(UoI_Total_Perc))

uoi_bottom_10 <- infographic_data_no_total |>
  select(County, UoI_Total_Perc) |>
  top_n(-10) |>
  arrange(desc(UoI_Total_Perc))

uoi_bottom_total <- infographic_data |>
  filter(County == "Kenya") |>
select(UoI_Total_Perc)

# g) Stand Alone Radio (%)

radio_top_10 <- infographic_data_no_total |>
  select(County, StandAloneRadio) |>
  top_n(10) |>
  arrange(desc(StandAloneRadio))

radio_bottom_10 <- infographic_data_no_total |>
  select(County, StandAloneRadio) |>
  top_n(-10) |>
  arrange(desc(StandAloneRadio))

radio_total <- infographic_data |>
  filter(County == "Kenya") |>
select(StandAloneRadio)

# h) TV (%)

tv_top_10 <- infographic_data_no_total |>
  select(County, FunctionalTV) |>
  top_n(10) |>
  arrange(desc(FunctionalTV))

tv_bottom_10 <- infographic_data_no_total |>
  select(County, FunctionalTV) |>
  top_n(-10) |>
  arrange(desc(FunctionalTV))

tv_total <- infographic_data |>
  filter(County == "Kenya") |>
select(FunctionalTV)

# h) Car (%)

car_top_10 <- infographic_data_no_total |>
  select(County, Car) |>
  top_n(10) |>
  arrange(desc(Car))

car_bottom_10 <- infographic_data_no_total |>
  select(County, Car) |>
  top_n(-10) |>
  arrange(desc(Car))

car_total <- infographic_data |>
  filter(County == "Kenya") |>
select(Car)
