# Solid Waste Disposal in Kenya

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

solid_waste_disp <- V4_T2.17

write.csv(solid_waste_disp, "sub_pro_17_waste_disposal/datasets/solid_waste_disp.csv")
