# Fun facts about County Names

# Load the libraries

library(tidyverse)
library(readxl)
library(janitor)

# Load the data

kenya_fun_facts <- read_excel("sub_pro_1_kenya_county_sub_county/kenya_fun_facts/datasets/kenya_fun_facts_1.xlsx")

# Clean names
kenya_fun_facts <- kenya_fun_facts |>
  clean_names()

# Create new columns
kenya_fun_facts <- kenya_fun_facts |>
  mutate(match = if_else(county == headquarters, "Yes", "No"),
         two_word_county = if_else(str_count(county, "\\S+") == 1, "Single", "Double"),
         two_word_hq = if_else(str_count(headquarters, "\\S+") == 1, "Single", "Double"))
