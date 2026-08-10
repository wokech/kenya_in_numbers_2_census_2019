# Households growing permanent crops
# Agricultural Census

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

perm_crops_hh <- V4_T2.21

write.csv(perm_crops_hh, "sub_pro_15_general_farming/datasets/perm_crops_hh.csv")

