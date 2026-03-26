# E-commerce Kenya

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

e_commerce <- V4_T2.34

write.csv(e_commerce, "sub_pro_14_e_commerce/datasets/e_commerce.csv")

