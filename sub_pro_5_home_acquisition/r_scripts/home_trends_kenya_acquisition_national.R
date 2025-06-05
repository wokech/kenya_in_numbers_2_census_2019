# Mode of acquisition for the houses that have been purchased
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_1 <- V4_T2.11a

View(df_1)


# 4) Preliminary filtering and cleanup

# Table 1 for National Analysis
table_1 <- df_1[1:3,]
View(table_1)

table_1 <- table_1 %>%
  clean_names()

table_1_select <- table_1 %>%
  select(county, sub_county, constructed_perc, purchased_perc, inherited_perc) 

View(table_1_select)

# 5) ggplot2 visualization

# Treemap

#install.packages("treemapify")
library(treemapify)

table_1_select_tidy <- table_1_select %>%
  pivot_longer(c(constructed_perc, purchased_perc, inherited_perc), 
               names_to = "acqui_mode", values_to = "percentage") %>%
  mutate(acqui_mode = ifelse(acqui_mode == "constructed_perc", "Constructed",
                        ifelse(acqui_mode == "purchased_perc", "Purchased",
                         ifelse(acqui_mode == "inherited_perc", "Inherited", acqui_mode))))
table_1_select_tidy

table_1_select_tidy_national <- table_1_select_tidy[1:3,]

# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_national, 
       aes(area = percentage, fill = acqui_mode, 
           label = paste(percentage,"%" , sep =""))) +
           geom_treemap() +
  labs(fill = "Property\nacquisition\nmode") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 20) 

ggsave("images/acqui_national/treemap_acqui_plot.png", width = 12, height = 8)

# Donut maps

#### Also use ggrepel to prevent overlap of labels
library(ggrepel)

table_1_select_tidy_urb <- table_1_select_tidy %>%
  filter(sub_county == "URBAN")
table_1_select_tidy_rur <- table_1_select_tidy %>%
  filter(sub_county == "RURAL")

# Hole size
hsize <- 3

table_1_select_tidy_urb <- table_1_select_tidy_urb %>% 
  mutate(x = hsize)

table_1_select_tidy_rur <- table_1_select_tidy_rur %>% 
  mutate(x = hsize)

# Urban Property Acquisition Mode

ggplot(table_1_select_tidy_urb, aes(x = hsize, y = percentage, fill = acqui_mode)) +
  geom_col(color = "black") +
  geom_label_repel(aes(label = paste(percentage, "%" , sep ="")),
            position = position_stack(vjust = 0.25), size = 5) +
  coord_polar("y", start=0) +
  xlim(c(0.2, hsize + 0.5)) +
  labs(fill = "Property\nacquisition\nmode") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_legend(override.aes = aes(color = NA)))

ggsave("images/acqui_national/donut_acqui_plot_urb.png", width = 5, height = 5)

# Rural Property Acquisition Mode

ggplot(table_1_select_tidy_rur, aes(x = hsize, y = percentage, fill = acqui_mode)) +
  geom_col(color = "black") +
  geom_label_repel(aes(label = paste(percentage, "%" , sep ="")),
            position = position_stack(vjust = 0.25), size = 5) +
  coord_polar("y", start=0) +
  xlim(c(0.2, hsize + 0.5)) +
  labs(fill = "Property\nacquisition\nmode") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_legend(override.aes = aes(color = NA)))

ggsave("images/acqui_national/donut_acqui_plot_rur.png", width = 5, height = 5)
