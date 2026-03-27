# Albinism in Kenya

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

pop_albinism <- V4_T2.28

write.csv(pop_albinism, "sub_pro_16_disability/datasets/pop_albinism.csv")
