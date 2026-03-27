# Disability by Type in Kenya

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

pop_disability_type <- V4_T2.27

write.csv(pop_disability_type, "sub_pro_16_disability/datasets/pop_disability_type.csv")
