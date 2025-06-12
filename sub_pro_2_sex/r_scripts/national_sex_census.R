# # Does Kenya have more men than women?
# By @afro_dataviz
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
library(scales) # control axis/scale format
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot) # plotting theme
library(gghighlight) # highlight specific data

# 2) View the data available in the data catalogue

data("DataCatalogue") # listing of all the available census 2019 datasets

# 3) Load the required data for the male-female numbers plot

df_pop <- V1_T2.2 # Load the "Distribution of Population by Sex and County" dataset
df_pop_national <- df_pop[1,] # isolate the national male:female numbers
df_pop_national_tidy <- df_pop_national |>
  pivot_longer(!County, names_to = "Gender", values_to = "Number") #tidy the dataset and pivot longer
df_pop_national_tidy <- df_pop_national_tidy[1:2,] # remove intersex and total

# 4) Plot the male-female numbers plot

ggplot(df_pop_national_tidy, aes(County, Number, fill = Gender)) + 
  geom_bar(position="stack", stat="identity", width = 2) + 
  coord_flip() + 
  theme_void() + 
  scale_fill_manual(values = c("#f4c2c2", "#89CFF0")) +
  labs(x = "", 
       y = "", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "")+
  geom_text(aes(label=comma(Number)), color="black", size=10, vjust = 1, position = position_stack(vjust=0.5)) +
  geom_text(aes(label=Gender), color="black", size=10, vjust = -1, position = position_stack(vjust=0.5)) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"))  

#ggsave("sub_pro_2_sex/images/male_to_female.png", width = 8, height = 8, dpi = 300)

# 5) Load the required data for the male-female by age plot

df_age_sex_ratio <- V3_T2.2

df_age_sex_ratio_filter <- df_age_sex_ratio |> # drop the unnecessary groupings
  filter(!grepl('0-|5-|100|Total|Not', Age))
df_age_sex_ratio_filter$Age <- as.numeric(as.character(df_age_sex_ratio_filter$Age)) #change age to numeric
glimpse(df_age_sex_ratio_filter)
df_age_sex_ratio_filter_select <- df_age_sex_ratio_filter |> # calculate male:female ratio
  mutate(m_f_ratio = round(Male *100/Female)) |>
  select(Age, m_f_ratio)

# # Age sets for analysis if necessary
# df_age_sex_ratio_filter_select_0_19 <- df_age_sex_ratio_filter_select[1:19,]
# df_age_sex_ratio_filter_select_20_35 <- df_age_sex_ratio_filter_select[20:35,]
# df_age_sex_ratio_filter_select_36_57 <- df_age_sex_ratio_filter_select[36:57,]
# df_age_sex_ratio_filter_select_58_100 <- df_age_sex_ratio_filter_select[58:100,]

df_age_sex_ratio_plot <- df_age_sex_ratio_filter_select|>
  ggplot(aes(Age, m_f_ratio)) + 
  geom_point(color = "red", size = 2.5) + 
  geom_rect(aes(xmin = -0.5, xmax = 19.5, ymin = -Inf, ymax = Inf),
            fill = "goldenrod1", alpha = 0.005, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 35.5, xmax = 57.5, ymin = -Inf, ymax = Inf),
            fill = "goldenrod1", alpha = 0.005, inherit.aes = FALSE) +
  theme_classic() +
  labs(x = "Age (yrs)", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20, vjust = 2),
        axis.text.x =element_text(size = 20),
        axis.text.y =element_text(size = 20),
        plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "Helvetica",size = 12),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        plot.background = element_rect(fill = "azure2", colour = "azure2")) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5) +
  geom_text(aes(x = 85 , y = 100, label = "Male:Female ratio = 1:1"), size = 6, angle=0, vjust = -1) +
  geom_text(aes(x = 9.5 , y = 50, label = "Age Range:\n0-19 yrs"), size = 6, angle=0) +
  geom_text(aes(x = 46.5 , y = 50, label = "Age Range:\n36-57 yrs"), size = 6, angle=0)

df_age_sex_ratio_plot

# ggsave("sub_pro_2_sex/images/male_to_female_per_age.png", width = 8, height = 8, dpi = 300)

