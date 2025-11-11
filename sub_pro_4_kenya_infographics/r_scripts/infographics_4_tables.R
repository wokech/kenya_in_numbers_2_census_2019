# Infographics Tables 4
# By @kenya.in.numbers
# From the Kenya Population and Housing Census Report (2019) and rKenyaCensus

#####################
#####PART A
#####################

# Load all the required packages and libraries required for accessing the census data

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
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

# View the data available in the data catalogue

data("DataCatalogue")

# b) Clean the tables in V4. Table 2.5, 2.15, 2.16, 2.18, and 2.19

df_v4_t2_15 <- V4_T2.15 # Main Source of Drinking Water
df_v4_t2_16 <- V4_T2.16 #  Main Mode of Human Waste Disposal
df_v4_t2_18 <- V4_T2.18 # Main Type of Cooking Fuel
df_v4_t2_19 <- V4_T2.19 # Main Type of Lighting Fuel

# Select appropriate columns and rows

# Main Source of Drinking Water

df_v4_t2_15 <- df_v4_t2_15 |>
  filter(AdminArea == "County" | SubCounty == "KENYA")

unique(df_v4_t2_15$County)
df_v4_t2_15$County <- gsub("/", " ", df_v4_t2_15$County)
df_v4_t2_15$County <- gsub("-", " ", df_v4_t2_15$County)
df_v4_t2_15$County <- gsub("xxx", "Kenya", df_v4_t2_15$County)

df_v4_t2_15 <- df_v4_t2_15 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_15$County)

#  Main Mode of Human Waste Disposal

df_v4_t2_16 <- df_v4_t2_16 |>
  filter(AdminArea == "County" | SubCounty == "KENYA")

unique(df_v4_t2_16$County)
df_v4_t2_16$County <- gsub("/", " ", df_v4_t2_16$County)
df_v4_t2_16$County <- gsub("-", " ", df_v4_t2_16$County)
df_v4_t2_16$County <- gsub("xxx", "Kenya", df_v4_t2_16$County)

df_v4_t2_16 <- df_v4_t2_16 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_16$County)

# Main Type of Cooking Fuel

df_v4_t2_18 <- df_v4_t2_18 |>
  filter(AdminArea == "County" | SubCounty == "KENYA")

unique(df_v4_t2_18$County)
df_v4_t2_18$County <- gsub("/", " ", df_v4_t2_18$County)
df_v4_t2_18$County <- gsub("-", " ", df_v4_t2_18$County)
df_v4_t2_18$County <- gsub("xxx", "Kenya", df_v4_t2_18$County)

df_v4_t2_18 <- df_v4_t2_18 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_18$County)

# Main Type of Lighting Fuel

df_v4_t2_19 <- df_v4_t2_19 |>
  filter(AdminArea == "County" | SubCounty == "KENYA")

unique(df_v4_t2_19$County)
df_v4_t2_19$County <- gsub("/", " ", df_v4_t2_19$County)
df_v4_t2_19$County <- gsub("-", " ", df_v4_t2_19$County)
df_v4_t2_19$County <- gsub("xxx", "Kenya", df_v4_t2_19$County)

df_v4_t2_19 <- df_v4_t2_19 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_19$County)

# Select the required columns

df_v4_t2_15 <- df_v4_t2_15 |>
  clean_names() |>
  select(county:conventional_households, pipedintodwelling, pipedtoyard_plot) |>
  mutate(total_piped = pipedintodwelling + pipedtoyard_plot)

df_v4_t2_16 <- df_v4_t2_16 |>
  clean_names() |>
  select(county:conventional_households, main_sewer, septic_tank, 
         bio_septic_tank_biodigester, open_bush, cess_pool) |>
  mutate(total_flush = main_sewer + septic_tank + 
           bio_septic_tank_biodigester + cess_pool)

df_v4_t2_18 <- df_v4_t2_18 |>
  clean_names() |>
  select(county:conventional_households, gas_lpg, biogas, firewood, charcoal) |>
  mutate(total_gas = gas_lpg + biogas, total_wood = firewood + charcoal)

df_v4_t2_19 <- df_v4_t2_19 |>
  clean_names() |>
  select(county:conventional_households, mains_electricity, solar, 
         torch_spotlight_solar_charged, torch_spotlight_dry_cells,
         paraffin_pressure_lamp, paraffin_lantern, paraffin_tin_lamp) |>
  mutate(total_torch = torch_spotlight_solar_charged + torch_spotlight_dry_cells,
         total_paraffin = paraffin_lantern + paraffin_pressure_lamp + paraffin_tin_lamp)


setdiff(df_v4_t2_15$county, df_v4_t2_16$county)

merged_table_total_15_16_18_19 <- df_v4_t2_15 |>
  left_join(df_v4_t2_16, by = c("county", "sub_county", "admin_area", "conventional_households")) |>
  left_join(df_v4_t2_18, by = c("county", "sub_county", "admin_area", "conventional_households")) |>
  left_join(df_v4_t2_19, by = c("county", "sub_county", "admin_area", "conventional_households")) |>
  select(-c(pipedintodwelling, pipedtoyard_plot, main_sewer, septic_tank,
            paraffin_pressure_lamp, paraffin_lantern, paraffin_tin_lamp,
            torch_spotlight_solar_charged, torch_spotlight_dry_cells,
            gas_lpg, biogas, firewood, charcoal, bio_septic_tank_biodigester))

write_csv(merged_table_total_15_16_18_19,
          "sub_pro_4_kenya_infographics/datasets/infographic_data_4.csv")


#####################
#####PART B - Visualize Data
#####################