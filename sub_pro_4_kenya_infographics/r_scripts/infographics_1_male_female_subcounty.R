# Infographic 1: Male vs Female (SubCounty Level)
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
  filter(AdminArea == "SubCounty") |>
  filter(!str_detect(SubCounty, "FOREST|PARK")) |>
  select(County, SubCounty, MPO_Total_Perc, MPO_Male_Perc, MPO_Female_Perc)

unique(df_v4_t2_32$County)
df_v4_t2_32$County <- gsub("/", " ", df_v4_t2_32$County)
df_v4_t2_32$County <- gsub("-", " ", df_v4_t2_32$County)
df_v4_t2_32 <- df_v4_t2_32 |> 
  mutate(County = tools::toTitleCase(tolower(County)))
unique(df_v4_t2_32$County)

unique(df_v4_t2_32$SubCounty)
df_v4_t2_32$SubCounty <- gsub("/", " ", df_v4_t2_32$SubCounty)
df_v4_t2_32$SubCounty <- gsub("-", " ", df_v4_t2_32$SubCounty)
df_v4_t2_32 <- df_v4_t2_32 |> 
  mutate(SubCounty = tools::toTitleCase(tolower(SubCounty)))
unique(df_v4_t2_32$SubCounty)

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

# Find differences in county names

setdiff(df_v4_t2_32$County, df_v4_t2_33$County)
setdiff(df_v4_t2_33$County, df_v4_t2_32$County)

# Find differences in subcounty names

setdiff(df_v4_t2_32$SubCounty, df_v4_t2_33$SubCounty)
setdiff(df_v4_t2_33$SubCounty, df_v4_t2_32$SubCounty)

# Create tables with relevant parameters at the county level

# Mobile Phone Ownership (%)

mpo_subcounty <- df_v4_t2_32 |>
  mutate(m_f_diff_perc = MPO_Male_Perc - MPO_Female_Perc) |>
  clean_names()

# Internet Usage (%)

uoi_subcounty <- df_v4_t2_33 |>
  mutate(uoi_diff_perc = UoI_Male_Perc - UoI_Female_Perc) |>
  select(County, SubCounty, UoI_Total_Perc, UoI_Male_Perc, UoI_Female_Perc, uoi_diff_perc) |>
  clean_names()

# Use of Desktop/Laptop/Tablet (%)

uodlt_subcounty <- df_v4_t2_33 |>
  mutate(uodlt_diff_perc = UoDLT_Male_Perc - UoDLT_Female_Perc) |>
  select(County, SubCounty, UoDLT_Total_Perc, UoDLT_Male_Perc, UoDLT_Female_Perc, uodlt_diff_perc) |>
  clean_names()


################################################################################
# Part B: Data Visualization
################################################################################

# Mobile Phone Ownership (%)

# Find the top 10 subcounties

top_subcounty_mpo <- mpo_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(mpo_total_perc)) |>
  slice(1:10)

View(top_subcounty_mpo)

# Find the bottom 10 subcounties

bottom_subcounty_mpo <- mpo_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(mpo_total_perc) |>
  slice(1:10) |>
  arrange(desc(mpo_total_perc))

View(bottom_subcounty_mpo)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_mpo <- rbind(top_subcounty_mpo, bottom_subcounty_mpo)
View(merge_top_bottom_subcounty_mpo)


ggplot(merge_top_bottom_subcounty_mpo) +
  geom_segment(aes(x = mpo_female_perc, xend = mpo_male_perc, 
                   y = reorder(county_sub, mpo_total_perc), yend = reorder(county_sub, mpo_total_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = mpo_male_perc, y = reorder(county_sub, mpo_total_perc)), color = "navy", size = 5) +
  geom_point(aes(x = mpo_female_perc, y = reorder(county_sub, mpo_total_perc)), color = "salmon", size = 5) +
  scale_x_continuous() +
  labs(x = "Mobile Phone Ownership (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24),
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female_subcounty/mpo_male_female_subcounty.png",
       width = 12, height = 12, dpi = 300)



# Internet Usage (%)

# Find the top 10 subcounties

top_subcounty_uoi <- uoi_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(uo_i_total_perc)) |>
  slice(1:10)

View(top_subcounty_uoi)

# Find the bottom 10 subcounties

bottom_subcounty_uoi <- uoi_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(uo_i_total_perc) |>
  slice(1:10) |>
  arrange(desc(uo_i_total_perc))

View(bottom_subcounty_uoi)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_uoi <- rbind(top_subcounty_uoi, bottom_subcounty_uoi)
View(merge_top_bottom_subcounty_uoi)


ggplot(merge_top_bottom_subcounty_uoi) +
  geom_segment(aes(x = uo_i_female_perc, xend = uo_i_male_perc, 
                   y = reorder(county_sub, uo_i_total_perc), yend = reorder(county_sub, uo_i_total_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = uo_i_male_perc, y = reorder(county_sub, uo_i_total_perc)), color = "navy", size = 5) +
  geom_point(aes(x = uo_i_female_perc, y = reorder(county_sub, uo_i_total_perc)), color = "salmon", size = 5) +
  scale_x_continuous() +
  labs(x = "Internet Usage (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24),
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female_subcounty/uoi_male_female_subcounty.png",
       width = 12, height = 12, dpi = 300)



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
  theme(axis.title.x =element_text(size = 24),
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female_subcounty/uodlt_male_female_subcounty.png",
       width = 12, height = 12, dpi = 300)


# Plot top 10 and bottom 10 Male-Female Differences in percentage


# Mobile Phone Ownership (%)

# Find the top 10 subcounties

top_subcounty_mpo_diff <- mpo_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(m_f_diff_perc)) |>
  slice(1:10)

View(top_subcounty_mpo_diff)

# Find the bottom 10 subcounties

bottom_subcounty_mpo_diff <- mpo_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(m_f_diff_perc) |>
  slice(1:10) |>
  arrange(desc(m_f_diff_perc))

View(bottom_subcounty_mpo_diff)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_mpo_diff <- rbind(top_subcounty_mpo_diff, bottom_subcounty_mpo_diff)
View(merge_top_bottom_subcounty_mpo_diff)


ggplot(merge_top_bottom_subcounty_mpo_diff) +
  geom_segment(aes(x = 0, xend = m_f_diff_perc, 
                   y = reorder(county_sub, m_f_diff_perc), yend = reorder(county_sub, m_f_diff_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = m_f_diff_perc, y = reorder(county_sub, m_f_diff_perc)), color = "purple", size = 5) +
  scale_x_continuous() +
  labs(x = "Magnitude of sex differences in mobile phone ownership (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24),
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female_subcounty/mpo_male_female_subcounty_diff.png",
       width = 12, height = 12, dpi = 300)



# Internet Usage (%)

# Find the top 10 subcounties

top_subcounty_uoi_diff <- uoi_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(uoi_diff_perc)) |>
  slice(1:10)

View(top_subcounty_uoi_diff)

# Find the bottom 10 subcounties

bottom_subcounty_uoi_diff <- uoi_subcounty |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(uoi_diff_perc) |>
  slice(1:10) |>
  arrange(desc(uoi_diff_perc))

View(bottom_subcounty_uoi_diff)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_uoi_diff <- rbind(top_subcounty_uoi_diff, bottom_subcounty_uoi_diff)
View(merge_top_bottom_subcounty_uoi_diff)


ggplot(merge_top_bottom_subcounty_uoi_diff) +
  geom_segment(aes(x = 0, xend = uoi_diff_perc, 
                   y = reorder(county_sub, uoi_diff_perc), yend = reorder(county_sub, uoi_diff_perc)), 
               color = "purple", linewidth = 2) +
  geom_point(aes(x = uoi_diff_perc, y = reorder(county_sub, uoi_diff_perc)), color = "purple", size = 5) +
  scale_x_continuous() +
  labs(x = "Magnitude of sex differences in internet usage (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24),
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female_subcounty/uoi_male_female_subcounty_diff.png",
       width = 12, height = 12, dpi = 300)



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
  labs(x = "Magnitude of sex differences in desktop, laptop, or tablet use (%)", y = "",
       title = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 24),
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

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_male_female_subcounty/uodlt_male_female_subcounty_diff.png",
       width = 12, height = 12, dpi = 300)
