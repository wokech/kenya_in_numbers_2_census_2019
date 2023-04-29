# # Does Kenya have more men than women?
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
library(scales)
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_pop <- V1_T2.2
df_pop_national <- df_pop[1,]
df_pop_national_tidy <- df_pop_national %>%
  pivot_longer(!County, names_to = "Gender", values_to = "Number")
df_pop_national_tidy <- df_pop_national_tidy 
df_pop_national_tidy <- df_pop_national_tidy[1:2,] 

ggplot(df_pop_national_tidy, aes(County, Number, fill = Gender)) + 
  geom_bar(position="stack", stat="identity", width = 2) + 
  coord_flip() + 
  theme_void() + 
  scale_fill_manual(values = c("yellow", "blue")) +
  labs(x = "", 
       y = "", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "")+
  geom_text(aes(label=comma(Number)),color="black",size=10,position=position_stack(vjust=0.5)) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        legend.position = "right",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "bisque", colour = "bisque"),
        plot.background = element_rect(fill = "bisque", colour = "bisque"))  

ggsave("images/national_sex_census/sex_ratio.png", width = 8, height = 4)

df_age_sex_ratio <- V3_T2.2

df_age_sex_ratio_filter <- df_age_sex_ratio %>%
  filter(!grepl('0-|5-|100|Total|Not', Age))
df_age_sex_ratio_filter$Age <- as.numeric(as.character(df_age_sex_ratio_filter$Age))
glimpse(df_age_sex_ratio_filter)
df_age_sex_ratio_filter_select <- df_age_sex_ratio_filter %>%
  mutate(m_f_ratio = round(Male *100/Female)) %>%
  select(Age, m_f_ratio)

# Age sets for analysis if necessary
df_age_sex_ratio_filter_select_0_19 <- df_age_sex_ratio_filter_select[1:19,]
df_age_sex_ratio_filter_select_20_35 <- df_age_sex_ratio_filter_select[20:35,]
df_age_sex_ratio_filter_select_36_57 <- df_age_sex_ratio_filter_select[36:57,]
df_age_sex_ratio_filter_select_58_100 <- df_age_sex_ratio_filter_select[58:100,]

df_age_sex_ratio_plot <- df_age_sex_ratio_filter_select%>%
  ggplot(aes(Age, m_f_ratio)) + 
  geom_line(color = "darkblue", size = 1.5) + 
  theme_classic() +
  labs(x = "Age (yrs)", 
       y = "Number of males per 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 14),
        axis.title.y =element_text(size = 14, vjust = 2),
        axis.text.x =element_text(size = 14),
        axis.text.y =element_text(size = 14),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5) +
  geom_text(aes(x = 75 , y = 100, label = "Male:Female ratio = 1:1"), size = 4, angle=0, vjust = -1)

df_age_sex_ratio_plot

ggsave("images/national_sex_census/age_sex_ratio.png", width = 10, height = 6)

