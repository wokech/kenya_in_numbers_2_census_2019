# Livestock populations

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

livestock_pop_all <- V4_T2.24

write.csv(livestock_pop_all, "sub_pro_15_general_farming/datasets/livestock_pop_all.csv")
