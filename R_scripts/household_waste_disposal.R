# R Kenya Census - Main mode of Human 
# Waste Disposal by Area of Residence

## Author: William Okech

# Required packages
library(tidyverse)
install.packages(c("janitor", "waffle"))
library(janitor)
library(waffle)
library(ggsci)
library(ggpubr)
library(scales)

# Install the rKenyaCensus package by Shelmith Kariuki

install.packages("remotes")
remotes::install_github("Shelmith-Kariuki/rKenyaCensus")

# Load the package
library(rKenyaCensus)

# Look for the data in the catalogue
data("DataCatalogue")

# Load the dataframe
df <- V4_T2.16
View(df)

# Clean the whole dataset

df_clean <- df %>% clean_names()
df_clean

# Clean names in this mini-dataset
urb_rur <- df[1:3,]
urb_rur_df <- urb_rur %>% clean_names()

# Remove the "county" and "admin_area" columns
urb_rur_df_1 <- urb_rur_df %>% 
  ungroup() %>% 
  select(-county, -admin_area)

# Rename column 1
colnames(urb_rur_df_1)[1] <- "location"

# Pivot the data
urb_rur_df_2 <- urb_rur_df_1 %>% 
  pivot_longer(-c("location","conventional_households"), 
               names_to = "waste_disp_method", 
               values_to = "percentage")

# Plot the data

urb_rur_df_2 %>% 
  arrange(percentage) %>% 
  ggplot(aes(waste_disp_method, percentage, fill = location)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip()


county_df <- df_clean %>%
  filter(admin_area == "County")

