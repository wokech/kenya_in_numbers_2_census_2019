# Infographics Tables
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

# a) Merge the tables in V1. Table 2.2 to 2.4

df_v1_t2_2 <- V1_T2.2 # male/female
df_v1_t2_3 <- V1_T2.3 # households
df_v1_t2_4 <- V1_T2.4 # land area and pop density

# Clean table 2.2

unique(df_v1_t2_2$County)
df_v1_t2_2$County <- gsub("/", " ", df_v1_t2_2$County)
df_v1_t2_2$County <- gsub("-", " ", df_v1_t2_2$County)
df_v1_t2_2 <- df_v1_t2_2 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v1_t2_2$County)

df_v1_t2_2 <- df_v1_t2_2 |>
  mutate(County = str_trim(County))  # removes leading & trailing spaces

# Clean table 2.3

unique(df_v1_t2_3$County)
df_v1_t2_3$County <- gsub("/", " ", df_v1_t2_3$County)
df_v1_t2_3$County <- gsub("-", " ", df_v1_t2_3$County)
df_v1_t2_3 <- df_v1_t2_3 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v1_t2_3$County)

df_v1_t2_3 <- df_v1_t2_3 |>
  mutate(County = str_trim(County))  # removes leading & trailing spaces

# Clean table 2.4

unique(df_v1_t2_4$County)
df_v1_t2_4$County <- gsub("/", " ", df_v1_t2_4$County)
df_v1_t2_4$County <- gsub("-", " ", df_v1_t2_4$County)
df_v1_t2_4 <- df_v1_t2_4 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v1_t2_4$County)

df_v1_t2_4 <- df_v1_t2_4 |>
  mutate(County = str_trim(County))  # removes leading & trailing spaces

# Examine the differences in the County column values

setdiff(df_v1_t2_2$County, df_v1_t2_3$County)
setdiff(df_v1_t2_3$County, df_v1_t2_2$County)

setdiff(df_v1_t2_2$County, df_v1_t2_4$County)
setdiff(df_v1_t2_4$County, df_v1_t2_2$County)

setdiff(df_v1_t2_3$County, df_v1_t2_4$County)
setdiff(df_v1_t2_4$County, df_v1_t2_3$County)

# Rename the values that don't match in each table

df_v1_t2_2 <- df_v1_t2_2 |>
  mutate(County = case_when(
    County == "Total" ~ "Kenya",
    TRUE ~ County  # keep other values unchanged
  )) |>
  mutate(M_F_Ratio_per_100 = round((Male/Female)*100, 0))

df_v1_t2_3 <- df_v1_t2_3 |>
  mutate(County = case_when(
    County == "Tanariver" ~ "Tana River",
    County == "Westpokot" ~ "West Pokot",
    County == "Transnzoia" ~ "Trans Nzoia",
    County == "Uasingishu" ~ "Uasin Gishu",
    County == "Homabay" ~ "Homa Bay",
    County == "Nairobicity" ~ "Nairobi City",
    TRUE ~ County  # keep other values unchanged
  )) |>
  select(-Population) # drop population column

df_v1_t2_4 <- df_v1_t2_4 |>
  mutate(County = case_when(
    County == "Tanariver" ~ "Tana River",
    County == "Westpokot" ~ "West Pokot",
    County == "Transnzoia" ~ "Trans Nzoia",
    County == "Uasingishu" ~ "Uasin Gishu",
    County == "Homabay" ~ "Homa Bay",
    County == "Nairobicity" ~ "Nairobi City",
    TRUE ~ County  # keep other values unchanged
  )) |>
  mutate(share_land_area = round((`LandArea(in Sq. Km)`/580876.3)*100, 3))

# Merge the tables

merged_table_1 <- df_v1_t2_2 %>%
  left_join(df_v1_t2_3, by = "County") %>%
  left_join(df_v1_t2_4, by = "County")

# b) Clean the tables in V4. Table 2.32 to 2.36

df_v4_t2_32 <- V4_T2.32
df_v4_t2_36 <- V4_T2.36

# Select appropriate columns and rows

df_v4_t2_32 <- df_v4_t2_32 |>
  filter(AdminArea == "County" | SubCounty == "KENYA") |>
  select(County, MPO_Total_Perc)

unique(df_v4_t2_32$County)
df_v4_t2_32$County <- gsub("/", " ", df_v4_t2_32$County)
df_v4_t2_32$County <- gsub("-", " ", df_v4_t2_32$County)
df_v4_t2_32$County <- gsub("xxx", "Kenya", df_v4_t2_32$County)
df_v4_t2_32 <- df_v4_t2_32 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_32$County)

df_v4_t2_36 <- df_v4_t2_36 |>
  filter(AdminArea == "County" | SubCounty == "Kenya") |>
  select(County, StandAloneRadio, FunctionalTV, Car, Internet)

unique(df_v4_t2_36$County)
df_v4_t2_36$County <- gsub("/", " ", df_v4_t2_36$County)
df_v4_t2_36$County <- gsub("-", " ", df_v4_t2_36$County)
df_v4_t2_36$County <- gsub("xxx", "Kenya", df_v4_t2_36$County)
df_v4_t2_36 <- df_v4_t2_36 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_36$County)

setdiff(df_v4_t2_32$County, df_v4_t2_36$County)
setdiff(df_v4_t2_36$County, df_v4_t2_32$County)

merged_table_2 <- df_v4_t2_32 |>
  left_join(df_v4_t2_36, by = "County")


setdiff(merged_table_1$County, merged_table_2$County)
setdiff(merged_table_2$County, merged_table_1$County)

# All tables merged

merged_table_total <- merged_table_1 |>
  left_join(merged_table_2, by = "County")

# write_csv(merged_table_total, 
#           "sub_pro_1_kenya_county_sub_county/kenya_infographics/datasets/infographic_data_1.csv")

#####################
#####PART B - Visualize Data
#####################

