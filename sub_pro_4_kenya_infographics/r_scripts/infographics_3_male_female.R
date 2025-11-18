# Infographic 3: Male vs Female
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

df_v4_t2_33 <- V4_T2.33 # Internet Usage (Distribution of Population Age 3 Years and Above Using Internet and Computer/Laptop/Tablet by Area of Residence, Sex, County and Sub-County)

# Select appropriate columns and rows

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

# Use of Desktop/Laptop/Tablet (%)

uodlt_county <- df_v4_t2_33 |>
  mutate(uodlt_diff_perc = UoDLT_Male_Perc - UoDLT_Female_Perc) |>
  select(County, UoDLT_Total_Perc, UoDLT_Male_Perc, UoDLT_Female_Perc, uodlt_diff_perc) |>
  clean_names()

################################################################################
# Part B: Data Visualization
################################################################################

# Use of Desktop/Laptop/Tablet (%)

ggplot(uodlt_county) +
  geom_segment(aes(x = uo_dlt_male_perc, xend = uo_dlt_female_perc, 
                   y = reorder(county, uo_dlt_total_perc), yend = reorder(county, uo_dlt_total_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = uo_dlt_male_perc, y = reorder(county, uo_dlt_total_perc)), color = "navy", size = 5) +
  geom_point(aes(x = uo_dlt_female_perc, y = reorder(county, uo_dlt_total_perc)), color = "salmon", size = 5) +
  scale_x_continuous() +
  labs(x = "Use of Desktop, Laptop, or Tablet (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24, margin = margin(t = 20)),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 18, color = "black"),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_male_female/uodlt_male_female.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# Plot Male-Female Differences in percentage
################################################################################

# Use of Desktop/Laptop/Tablet (%)

ggplot(uodlt_county) +
  geom_segment(aes(x = 0, 
                   xend = uodlt_diff_perc, 
                   y = reorder(county, uodlt_diff_perc), 
                   yend = reorder(county, uodlt_diff_perc),
                   color = m_f_diff_perc > 0), 
               linewidth = 2) +
  geom_point(aes(x = uodlt_diff_perc, 
                 y = reorder(county, uodlt_diff_perc), 
                 color = m_f_diff_perc > 0), 
                 size = 5) +
  scale_x_continuous() +
  scale_color_manual(
    values = c("TRUE" = "navy", "FALSE" = "salmon")) +
  labs(x = "Magnitude of sex differences in\ndesktop, laptop, or tablet use (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24, margin = margin(t = 20)),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 18, color = "black"),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        #plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_male_female/uodlt_male_female_diff.png",
       width = 12, height = 12, dpi = 300)

