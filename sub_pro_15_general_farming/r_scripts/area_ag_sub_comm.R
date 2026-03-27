# Area allocated to agriculture

# A) Load the required libraries

library(rKenyaCensus)
library(tidyverse)
library(janitor)

# B) Load the required datasets

area_ag_sub_comm <- V4_T2.25

write.csv(area_ag_sub_comm, "sub_pro_15_general_farming/datasets/area_ag_sub_comm.csv")
