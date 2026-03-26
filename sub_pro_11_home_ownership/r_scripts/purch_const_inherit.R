# Did they purchase, construct, or inherit?

# Distribution of Households Owning the Main Dwelling Unit 
# by Mode of Acquisition, Area of Residence, County and Sub-County

# By @kenya.in.numbers
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_purch_const_inherit <- V4_T2.11a
View(df_purch_const_inherit)

write.csv(df_purch_const_inherit, "sub_pro_11_home_ownership/datasets/purch_const_inherit_all.csv")

