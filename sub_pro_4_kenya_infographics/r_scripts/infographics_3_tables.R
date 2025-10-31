# Infographics Tables 1
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

# b) Clean the tables in V4. Table 2.33, and 2.36

df_v4_t2_33 <- V4_T2.33 # Internet Usage (Distribution of Population Age 3 Years and Above Using Internet and Computer/Laptop/Tablet by Area of Residence, Sex, County and Sub-County)
df_v4_t2_36 <- V4_T2.36 # Functional TV/Radio/Car (Percentage Distribution of Conventional Households by Ownership of Selected Household Assets by Area of Residence, County and Sub County)

# Select appropriate columns and rows

df_v4_t2_33 <- df_v4_t2_33 |>
  filter(AdminArea == "County" | SubCounty == "KENYA") |>
  select(County, UoDLT_Total_Perc)

unique(df_v4_t2_33$County)
df_v4_t2_33$County <- gsub("/", " ", df_v4_t2_33$County)
df_v4_t2_33$County <- gsub("-", " ", df_v4_t2_33$County)
df_v4_t2_33$County <- gsub("xxx", "Kenya", df_v4_t2_33$County)

df_v4_t2_33 <- df_v4_t2_33 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_33$County)

df_v4_t2_36 <- df_v4_t2_36 |>
  filter(AdminArea == "County" | SubCounty == "Kenya") |>
  select(County, "DeskTop/Computer/Laptop/Tablet", Bicycle,
         "Motor Cycle", Refrigerator, 
         "Truck/Lorry/Bus/Three Wheelertruck", "Tuk Tuk")

unique(df_v4_t2_36$County)
df_v4_t2_36$County <- gsub("/", " ", df_v4_t2_36$County)
df_v4_t2_36$County <- gsub("-", " ", df_v4_t2_36$County)
df_v4_t2_36$County <- gsub("xxx", "Kenya", df_v4_t2_36$County)
df_v4_t2_36 <- df_v4_t2_36 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_36$County)

setdiff(df_v4_t2_33$County, df_v4_t2_36$County)
setdiff(df_v4_t2_36$County, df_v4_t2_33$County)

merged_table_total_33_36 <- df_v4_t2_33 |>
  left_join(df_v4_t2_36, by = "County")

write_csv(merged_table_total_33_36,
          "sub_pro_4_kenya_infographics/datasets/infographic_data_3.csv")


#####################
#####PART B - Visualize Data
#####################