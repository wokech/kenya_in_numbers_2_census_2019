# Households growing non-permanent crops
# Agricultural Census

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

non_perm_crops_hh <- V4_T2.22

write.csv(non_perm_crops_hh, "sub_pro_15_general_farming/datasets/non_perm_crops_hh.csv")

