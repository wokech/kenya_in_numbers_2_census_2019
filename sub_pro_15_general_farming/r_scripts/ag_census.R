# Households practicing various forms of agriculture
# Agricultural Census

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

ag_census_hh <- V4_T2.20

write.csv(ag_census_hh, "sub_pro_15_general_farming/datasets/ag_census_hh.csv")
