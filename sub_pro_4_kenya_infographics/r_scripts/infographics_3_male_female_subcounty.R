# Infographic 3: Male vs Female (SubCounty Level)
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
  filter(AdminArea == "SubCounty") |>
  filter(!str_detect(SubCounty, "FOREST|PARK")) |>
  select(County, SubCounty, UoI_Total_Perc, UoI_Male_Perc, UoI_Female_Perc, 
         UoDLT_Total_Perc, UoDLT_Male_Perc, UoDLT_Female_Perc)

unique(df_v4_t2_33$County)
df_v4_t2_33$County <- gsub("/", " ", df_v4_t2_33$County)
df_v4_t2_33$County <- gsub("-", " ", df_v4_t2_33$County)
df_v4_t2_33 <- df_v4_t2_33 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_33$County)

unique(df_v4_t2_33$SubCounty)
df_v4_t2_33$SubCounty <- gsub("/", " ", df_v4_t2_33$SubCounty)
df_v4_t2_33$SubCounty <- gsub("-", " ", df_v4_t2_33$SubCounty)
df_v4_t2_33 <- df_v4_t2_33 |> 
  mutate(SubCounty = tools::toTitleCase(tolower(SubCounty)))
unique(df_v4_t2_33$SubCounty)

# Use of Desktop/Laptop/Tablet (%)

uodlt_subcounty <- df_v4_t2_33 |>
  mutate(uodlt_diff_perc = UoDLT_Male_Perc - UoDLT_Female_Perc) |>
  select(County, SubCounty, UoDLT_Total_Perc, UoDLT_Male_Perc, UoDLT_Female_Perc, uodlt_diff_perc) |>
  clean_names()


################################################################################
# Part B: Data Visualization
################################################################################


# Use of Desktop/Laptop/Tablet (%)

# Find the top 10 subcounties

top_subcounty_dlt <- uodlt_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(uo_dlt_total_perc)) |>
  slice(1:10)

View(top_subcounty_dlt)

# Find the bottom 10 subcounties

bottom_subcounty_dlt <- uodlt_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(uo_dlt_total_perc) |>
  slice(1:10) |>
  arrange(desc(uo_dlt_total_perc))

View(bottom_subcounty_dlt)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_dlt <- rbind(top_subcounty_dlt, bottom_subcounty_dlt)
View(merge_top_bottom_subcounty_dlt)


ggplot(merge_top_bottom_subcounty_dlt) +
  geom_segment(aes(x = uo_dlt_female_perc, xend = uo_dlt_male_perc, 
                   y = reorder(county_sub, uo_dlt_total_perc), yend = reorder(county_sub, uo_dlt_total_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = uo_dlt_male_perc, y = reorder(county_sub, uo_dlt_total_perc)), color = "navy", size = 5) +
  geom_point(aes(x = uo_dlt_female_perc, y = reorder(county_sub, uo_dlt_total_perc)), color = "salmon", size = 5) +
  scale_x_continuous() +
  labs(x = "Use of Desktop, Laptop, or Tablet (%)", y = "",
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_male_female_subcounty/uodlt_male_female_subcounty.png",
       width = 12, height = 12, dpi = 300)

# Plot top 10 and bottom 10 Male-Female Differences in percentage

# Use of Desktop/Laptop/Tablet (%)

# Find the top 10 subcounties

top_subcounty_dlt_diff <- uodlt_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(uodlt_diff_perc)) |>
  slice(1:10)

View(top_subcounty_dlt_diff)

# Find the bottom 10 subcounties

bottom_subcounty_dlt_diff <- uodlt_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(uodlt_diff_perc) |>
  slice(1:10) |>
  arrange(desc(uodlt_diff_perc))

View(bottom_subcounty_dlt_diff)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_dlt_diff <- rbind(top_subcounty_dlt_diff, bottom_subcounty_dlt_diff)
View(merge_top_bottom_subcounty_dlt_diff)


ggplot(merge_top_bottom_subcounty_dlt_diff) +
  geom_segment(aes(x = 0, xend = uodlt_diff_perc, 
                   y = reorder(county_sub, uodlt_diff_perc), yend = reorder(county_sub, uodlt_diff_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = uodlt_diff_perc, y = reorder(county_sub, uodlt_diff_perc)), color = "navy", size = 5) +
  scale_x_continuous() +
  labs(x = "Magnitude of sex differences in\ndesktop, laptop, or tablet use (%)", y = "",
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_male_female_subcounty/uodlt_male_female_subcounty_diff.png",
       width = 12, height = 12, dpi = 300)
