# Infographic 1: Male vs Female
# William Okech

################################################################################
# Part A: Setup and Data Wrangling 
################################################################################

# Load libraries

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(readxl)
library(janitor)
library(scales) # control axis/scale format
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot) # plotting theme
library(gghighlight) # highlight specific data
#install.packages("sf")
library(sf) # simple features
#install.packages("tmap") #Thematic maps 
library(tmap)
#install.packages("leaflet") # Used for creating interactive maps
library(leaflet)
#install.packages("ggbreak")
library(ggbreak)
library(patchwork)
library(ggrepel)
library(ggsflabel)

# View the data available in the data catalogue

data("DataCatalogue")

# Load the required tables

df_v4_t2_32 <- V4_T2.32 # Mobile Phone Ownership (Distribution of Population Age 3 years and Above Owning a Mobile Phone by Area of Residence, Sex, County and Sub County)
df_v4_t2_33 <- V4_T2.33 # Internet Usage (Distribution of Population Age 3 Years and Above Using Internet and Computer/Laptop/Tablet by Area of Residence, Sex, County and Sub-County)

# Select appropriate columns and rows

df_v4_t2_32 <- df_v4_t2_32 |>
  filter(AdminArea == "County") |>
  select(County, MPO_Total_Perc, MPO_Male_Perc, MPO_Female_Perc)

unique(df_v4_t2_32$County)
df_v4_t2_32$County <- gsub("/", " ", df_v4_t2_32$County)
df_v4_t2_32$County <- gsub("-", " ", df_v4_t2_32$County)
df_v4_t2_32$County <- gsub("xxx", "Kenya", df_v4_t2_32$County)
df_v4_t2_32 <- df_v4_t2_32 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_32$County)

df_v4_t2_33 <- df_v4_t2_33 |>
  filter(AdminArea == "County") |>
  select(County, UoI_Total_Perc, UoI_Male_Perc, UoI_Female_Perc, 
         UoDLT_Total_Perc, UoDLT_Male_Perc, UoDLT_Female_Perc)

unique(df_v4_t2_33$County)
df_v4_t2_33$County <- gsub("/", " ", df_v4_t2_33$County)
df_v4_t2_33$County <- gsub("-", " ", df_v4_t2_33$County)
df_v4_t2_33$County <- gsub("xxx", "Kenya", df_v4_t2_33$County)

df_v4_t2_33 <- df_v4_t2_33 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_33$County)

# Find differences in county names

setdiff(df_v4_t2_32$County, df_v4_t2_33$County)
setdiff(df_v4_t2_33$County, df_v4_t2_32$County)

# Create tables with relevant parameters at the county level

# Mobile Phone Ownership (%)

mpo_county <- df_v4_t2_32 |>
  mutate(m_f_diff_perc = MPO_Male_Perc - MPO_Female_Perc) |>
  clean_names()

# Internet Usage (%)

uoi_county <- df_v4_t2_33 |>
  mutate(uoi_diff_perc = UoI_Male_Perc - UoI_Female_Perc) |>
  select(County, UoI_Total_Perc, UoI_Male_Perc, UoI_Female_Perc, uoi_diff_perc) |>
  clean_names()

################################################################################
# Part B: Data Visualization
################################################################################

# Mobile Phone Ownership (%)

ggplot(mpo_county) +
  geom_segment(aes(x = mpo_female_perc, xend = mpo_male_perc, 
                   y = reorder(county, mpo_total_perc), yend = reorder(county, mpo_total_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = mpo_male_perc, y = reorder(county, mpo_total_perc)), color = "navy", size = 5) +
  geom_point(aes(x = mpo_female_perc, y = reorder(county, mpo_total_perc)), color = "salmon", size = 5) +
  scale_x_continuous() +
  labs(x = "Mobile Phone Ownership (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24, margin = margin(t = 20)),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 18),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female/mpo_male_female.png",
       width = 12, height = 12, dpi = 300)


# Internet Usage (%)

ggplot(uoi_county) +
  geom_segment(aes(x = uo_i_male_perc, xend = uo_i_female_perc, 
                   y = reorder(county, uo_i_total_perc), yend = reorder(county, uo_i_total_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = uo_i_male_perc, y = reorder(county, uo_i_total_perc)), color = "navy", size = 5) +
  geom_point(aes(x = uo_i_female_perc, y = reorder(county, uo_i_total_perc)), color = "salmon", size = 5) +
  scale_x_continuous() +
  labs(x = "Internet Usage (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24, margin = margin(t = 20)),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 18),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female/uoi_male_female.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# Plot Male-Female Differences in percentage
################################################################################

# Mobile Phone Ownership (%)

ggplot(mpo_county) +
  geom_segment(aes(x = 0, xend = m_f_diff_perc, 
                   y = reorder(county, m_f_diff_perc), yend = reorder(county, m_f_diff_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = m_f_diff_perc, y = reorder(county, m_f_diff_perc)), color = "purple", size = 5) +
  scale_x_continuous() +
  labs(x = "Magnitude of sex differences in\nmobile phone ownership (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24, margin = margin(t = 20)),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 18),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female/mpo_male_female_diff.png",
       width = 12, height = 12, dpi = 300)

# Internet Usage (%)

ggplot(uoi_county) +
  geom_segment(aes(x = 0, xend = uoi_diff_perc, 
                   y = reorder(county, uoi_diff_perc), yend = reorder(county, uoi_diff_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = uoi_diff_perc, y = reorder(county, uoi_diff_perc)), color = "purple", size = 5) +
  scale_x_continuous() +
  labs(x = "Magnitude of sex differences in\ninternet usage (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24, margin = margin(t = 20)),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 18),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female/uoi_male_female_diff.png",
       width = 12, height = 12, dpi = 300)