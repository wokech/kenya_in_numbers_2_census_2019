# Households rearing livestock
# Agricultural Census

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

livestock_hh <- V4_T2.23

write.csv(livestock_hh, "sub_pro_15_general_farming/datasets/livestock_hh.csv")

