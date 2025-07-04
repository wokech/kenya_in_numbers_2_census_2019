# Kenyan Generations County Splits
# Author: William Okech

################################################
# A. Load libraries
###############################################

# install.packages("readr")
# install.packages("patchwork")
# install.packages("ggthemes")
library(readr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggthemes)
library(scales)
# install.packages("devtools")
# devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)

#############################################################
# B. Load the required data from 2019 Census
############################################################

# Age-sex dataset
df_age_sex <- V3_T2.3
str(df_age_sex)

# Age-sex dataset for county

county_df_age_sex <- df_age_sex |>
  filter(SubCounty == "ALL") |>
  filter(Age != "Total" & Age != "Not Stated") |> 
  filter(!grepl("-", Age)) |>
  mutate(Age = if_else(Age == "100+", "100", Age))

# Age-sex dataset for subcounty

subcounty_df_age_sex <- df_age_sex |>
  filter(SubCounty != "ALL") |>
  filter(Age != "Total" & Age != "Not Stated") |> 
  filter(!grepl("-", Age)) |>
  mutate(Age = if_else(Age == "100+", "100", Age))
  
