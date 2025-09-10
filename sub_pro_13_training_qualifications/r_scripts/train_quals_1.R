# Distribution of Population Age 15 years and Above by 
# Sex and Main Training Acquired and Qualified for
# Census (2019)

# Data: rKenyaCensus

# Inspo: Data Story - Check Links

# Load libraries

library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

# Load data

train_quals <- V4_T2.7 

train_quals <- train_quals |>
  clean_names() |>
  mutate(m_f_ratio_100 = round(male*100/female))

