# Economic Growth Metrics (Combined)
# By @kenya.in.numbers
# From the Kenya Population and Housing Census Report (2019) and rKenyaCensus

# Econ growth is defined as access to goods and services of
# increasing quantity or quality.

#####################
#####PART A
#####################

# Load all the required packages and libraries required for accessing the census data

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(readxl)
library(janitor)

# Load the data

table_infographic_1 <- read_csv("sub_pro_4_kenya_infographics/datasets/infographic_data_1.csv")
table_infographic_3 <- read_csv("sub_pro_4_kenya_infographics/datasets/infographic_data_3.csv")
table_infographic_4 <- read_csv("sub_pro_4_kenya_infographics/datasets/infographic_data_4.csv")

# Clean names
table_infographic_1 <- table_infographic_1 %>%
  clean_names()
table_infographic_3 <- table_infographic_3 %>%
  clean_names()
table_infographic_4 <- table_infographic_4 %>%
  clean_names()

# Select the correct columns
table_infographic_1_select <- table_infographic_1 %>%
  select(county, average_household_size, population_density_no_per_sq_km, mpo_total_perc, 
         uo_i_total_perc, stand_alone_radio, functional_tv, car)
table_infographic_3_select <- table_infographic_3 %>%
  select(county, motor_cycle, bicycle, refrigerator, desk_top_computer_laptop_tablet)
table_infographic_4_select <- table_infographic_4 %>%
  select(county, total_piped, total_flush, total_gas, mains_electricity)

# Merge the data (3 at a time)

#put all data frames into list
table_infographic_list_merge <- list(table_infographic_1_select, 
                                     table_infographic_3_select, 
                                     table_infographic_4_select)      

#merge all data frames together
table_infographic_1_3_4_merged <- table_infographic_list_merge %>% 
  reduce(full_join, by='county')


# save the data

write_csv(table_infographic_1_3_4_merged,
          "sub_pro_4_kenya_infographics/datasets/table_infographic_1_3_4_merged.csv")
