# Disability in Kenya

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

pop_disability <- V4_T2.26

write.csv(pop_disability, "sub_pro_16_disability/datasets/pop_disability.csv")
