# What type of homes do Kenyans live in? Tenure, Mode of Acquisition, and House Type.
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_1 <- V4_T2.11a
View(df_1)

# 4) Preliminary filtering and cleanup

# Subcounty table
table_2_sc <- df_1[4:395,] %>%
  filter(AdminArea != "County")

# Remove unnecessary columns
table_2_sc_new <- table_2_sc %>%
  select(County, SubCounty, Purchased_Perc)

# Find the top 10 subcounties
top_subcounty <- table_2_sc_new %>%
  unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
  arrange(desc(Purchased_Perc)) %>%
  slice(1:20)
View(top_subcounty)

top_subcounty_plot <- top_subcounty %>%
  ggplot(aes(x = reorder(county_sub, Purchased_Perc), y = Purchased_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "chocolate3") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "SubCounty", 
       y = "Percentage (%) of households purchasing their home", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 10),
        axis.title.y =element_text(size = 10),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

top_subcounty_plot

ggsave("images/acqui_subcounty_purchase/top_subcounty_home_purchase_plot.png", width = 6, height = 4)

# Find the bottom 10 subcounties
bottom_subcounty <- table_2_sc_new %>%
  unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
  arrange(Purchased_Perc) %>%
  slice(1:20)
View(bottom_subcounty)


bottom_subcounty_plot <- bottom_subcounty %>%
  ggplot(aes(x = reorder(county_sub, Purchased_Perc), y = Purchased_Perc)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "chartreuse") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "SubCounty", 
       y = "Percentage (%) of households purchasing their home", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 10),
        axis.title.y =element_text(size = 10),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

bottom_subcounty_plot

ggsave("images/acqui_subcounty_purchase/bottom_subcounty_home_purchase_plot.png", width = 6, height = 4)

bottom_subcounty_plot + top_subcounty_plot 

ggsave("images/acqui_subcounty_purchase/top_bottom_home_purchase_plot.png", width = 12, height = 6)
