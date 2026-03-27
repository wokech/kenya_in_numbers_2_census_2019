# An analysis of religions in Kenya

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

religion <- V4_T2.30

# write.csv(religion, "sub_pro_13_religion/datasets/religion.csv")

View(V4_T2.21)
