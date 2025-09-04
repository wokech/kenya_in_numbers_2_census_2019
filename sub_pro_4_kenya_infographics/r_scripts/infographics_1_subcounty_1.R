# Subcounty Analysis - Infographics 1
# Population and  Sex Ratio

# Load libraries

library(patchwork)
library(tidyverse)
library(ggbreak)
library(rKenyaCensus)
library(janitor)
library(scales)

# NOTES

#################################
# Check for variables that differ
#################################

# unique(a$a)[which(!unique(a$a) %in% b$b)]

#################################

# 1) Load the subcounty sex census data and clean

infographic_1_pop <- V1_T2.5
infographic_1_pop

# Clean and filter out the relevant datasets

infographic_1_pop_clean <- infographic_1_pop |>
  clean_names()

# Filter out the total counties, parks, and forests

infographic_1_pop_clean <- infographic_1_pop_clean |>
  filter(sub_county != "Total") |>
  filter(admin_area != "County") |>
  filter(admin_area != "Special Area") |>
  mutate(county = str_remove(county, regex("\\bCounty\\b"))) |>
  mutate(across(where(is.character),str_squish))

################################################################################
# Male:Female Ratio
################################################################################

# Calculate the male:female ratio per 100
infographic_1_pop_clean_ratio <- infographic_1_pop_clean |>
  mutate(m_f_ratio = male/female,
         m_f_ratio_100 = round(m_f_ratio*100, 0))

# Find the top 10 subcounties (male:female ratio per 100)
top_subcounty_m_f_100 <- infographic_1_pop_clean_ratio |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(m_f_ratio_100)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_m_f_100)

# Find the bottom 10 subcounties (male:female ratio per 100)

bottom_subcounty_m_f_100 <- infographic_1_pop_clean_ratio |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(m_f_ratio_100) |>
  slice(1:10) |>
  arrange(desc(m_f_ratio_100)) |>
  mutate(bar_color = "Bottom")
  
View(bottom_subcounty_m_f_100)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_m_f_100 <- rbind(top_subcounty_m_f_100, bottom_subcounty_m_f_100)
View(merge_top_bottom_subcounty_m_f_100)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_sex_ratio <- merge_top_bottom_subcounty_m_f_100  |>
  ggplot(aes(x = reorder(county_sub, m_f_ratio_100), y = m_f_ratio_100, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = 0, label = county_sub), 
            color = "white", 
            fontface = "bold",
            size = 8,
            hjust = 0) +
  geom_text(aes(x = county_sub, y = m_f_ratio_100-4.5, label = m_f_ratio_100), 
            color = "white", 
            fontface = "bold",
            size = 8) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Number of males\nper 100 females", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_sex_ratio

# ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_sex_ratio.png", 
#        width = 12, height = 12, dpi = 300)

################################################################################
# Population
################################################################################

# Find the top 10 subcounties (Population)

top_subcounty_population <- infographic_1_pop_clean_ratio |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(total)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_population)

# Find the bottom 10 subcounties (Population)

bottom_subcounty_population <- infographic_1_pop_clean_ratio |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(total) |>
  slice(1:10) |>
  arrange(desc(total)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_population)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_population <- rbind(top_subcounty_population, bottom_subcounty_population)
View(merge_top_bottom_subcounty_population)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_population <- merge_top_bottom_subcounty_population  |>
  ggplot(aes(x = reorder(county_sub, total), y = total, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = total+100000, label = comma(total)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Population", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_population

# ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_population.png", 
#        width = 12, height = 12, dpi = 300)

################################################################################
# Other components of Infographic 1 (except Poverty(%) and GCP(%))
################################################################################

# Population and M/F Ratio are done on top
# Number of HH, Avg HH, Car, Internet Usage, Mobile Phone, Pop Density, Radio, and TV

################################################################################
# Number of Households
################################################################################

infographic_1_hh <- V1_T2.6
infographic_1_hh

infographic_1_hh_clean <- infographic_1_hh |>
  clean_names()

infographic_1_hh_clean <- infographic_1_hh_clean |>
  filter(sub_county != "Total") |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(county = str_remove(county, regex("\\bCounty\\b"))) |>
  mutate(across(where(is.character),str_squish))

# Find the top 10 subcounties (Number of Households)

top_subcounty_number_hh <- infographic_1_hh_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(number_of_households)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_number_hh)

# Find the bottom 10 subcounties (Number of Households)

bottom_subcounty_number_hh <- infographic_1_hh_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(number_of_households) |>
  slice(1:10) |>
  arrange(desc(number_of_households)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_number_hh)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_number_hh <- rbind(top_subcounty_number_hh, bottom_subcounty_number_hh)
View(merge_top_bottom_subcounty_number_hh)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_hh <- merge_top_bottom_subcounty_number_hh  |>
  ggplot(aes(x = reorder(county_sub, number_of_households), y = number_of_households, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = number_of_households+40000, label = comma(number_of_households)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Number of Households", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_hh

# ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_number_hh.png",
#        width = 12, height = 12, dpi = 300)

################################################################################
# Average Household Size
################################################################################

# Find the top 10 subcounties (Number of Households)

top_subcounty_avg_hh_size <- infographic_1_hh_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(average_household_size)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_avg_hh_size)

# Find the bottom 10 subcounties (Number of Households)

bottom_subcounty_avg_hh_size <- infographic_1_hh_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(average_household_size) |>
  slice(1:10) |>
  arrange(desc(average_household_size)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_avg_hh_size)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_avg_hh_size <- rbind(top_subcounty_avg_hh_size, bottom_subcounty_avg_hh_size)
View(merge_top_bottom_subcounty_avg_hh_size)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_avg_hh_size <- merge_top_bottom_subcounty_avg_hh_size  |>
  ggplot(aes(x = reorder(county_sub, average_household_size), y = average_household_size, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = average_household_size+0.5, label = comma(average_household_size)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Avg Household Size", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_avg_hh_size

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_avg_hh_size.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# Population Density
################################################################################

infographic_1_area_pop_density <- V1_T2.7
infographic_1_area_pop_density

infographic_1_area_pop_density_clean <- infographic_1_area_pop_density |>
  clean_names()

infographic_1_area_pop_density_clean <- infographic_1_area_pop_density_clean |>
  filter(sub_county != "Total") |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(county = str_remove(county, regex("\\bCounty\\b"))) |>
  mutate(across(where(is.character),str_squish))

# Find the top 10 subcounties (Population Density)

top_subcounty_pop_density <- infographic_1_area_pop_density_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(population_density_no_per_sq_km)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_pop_density)

# Find the bottom 10 subcounties (Population Density)

bottom_subcounty_pop_density <- infographic_1_area_pop_density_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(population_density_no_per_sq_km) |>
  slice(1:10) |>
  arrange(desc(population_density_no_per_sq_km)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_pop_density)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_pop_density <- rbind(top_subcounty_pop_density, bottom_subcounty_pop_density)
View(merge_top_bottom_subcounty_pop_density)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_pop_density <- merge_top_bottom_subcounty_pop_density  |>
  ggplot(aes(x = reorder(county_sub, population_density_no_per_sq_km), y = population_density_no_per_sq_km, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = population_density_no_per_sq_km+7500, label = comma(population_density_no_per_sq_km)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Population Density\n(per square kilometre)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_pop_density

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_pop_density.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# Area
################################################################################

# Find the top 10 subcounties (Area)

top_subcounty_area <- infographic_1_area_pop_density_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(land_area_in_sq_km)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_area)

# Find the bottom 10 subcounties (Area)

bottom_subcounty_area <- infographic_1_area_pop_density_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(land_area_in_sq_km) |>
  slice(1:10) |>
  arrange(desc(land_area_in_sq_km)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_area)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_area <- rbind(top_subcounty_area, bottom_subcounty_area)
View(merge_top_bottom_subcounty_area)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_area <- merge_top_bottom_subcounty_area  |>
  ggplot(aes(x = reorder(county_sub, land_area_in_sq_km), y = land_area_in_sq_km, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = land_area_in_sq_km+2000, label = comma(land_area_in_sq_km)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Area (square kilometres)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_area

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_area.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# Mobile Phone Ownership (%)
################################################################################

infographic_1_mpo <- V4_T2.32
infographic_1_mpo

infographic_1_mpo_clean <- infographic_1_mpo |>
  clean_names()

infographic_1_mpo_clean <- infographic_1_mpo_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Mobile Phone Ownership (%))

top_subcounty_mpo <- infographic_1_mpo_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(mpo_total_perc)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_mpo)

# Find the bottom 10 subcounties (Mobile Phone Ownership (%))

bottom_subcounty_mpo <- infographic_1_mpo_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(mpo_total_perc) |>
  slice(1:10) |>
  arrange(desc(mpo_total_perc)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_mpo)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_mpo <- rbind(top_subcounty_mpo, bottom_subcounty_mpo)
View(merge_top_bottom_subcounty_mpo)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_mpo <- merge_top_bottom_subcounty_mpo  |>
  ggplot(aes(x = reorder(county_sub, mpo_total_perc), y = mpo_total_perc, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = mpo_total_perc+7.5, label = comma(mpo_total_perc)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Mobile Phone Ownership (%)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_mpo

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_mpo.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# Internet Usage (%)
################################################################################

infographic_1_uoi <- V4_T2.33
infographic_1_uoi

infographic_1_uoi_clean <- infographic_1_uoi |>
  clean_names()

infographic_1_uoi_clean <- infographic_1_uoi_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Internet Usage (%))

top_subcounty_uoi <- infographic_1_uoi_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(uo_i_total_perc)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_uoi)

# Find the bottom 10 subcounties (Internet Usage (%))

bottom_subcounty_uoi <- infographic_1_uoi_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(uo_i_total_perc) |>
  slice(1:10) |>
  arrange(desc(uo_i_total_perc)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_uoi)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_uoi <- rbind(top_subcounty_uoi, bottom_subcounty_uoi)
View(merge_top_bottom_subcounty_uoi)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_uoi <- merge_top_bottom_subcounty_uoi  |>
  ggplot(aes(x = reorder(county_sub, uo_i_total_perc), y = uo_i_total_perc, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = uo_i_total_perc+5, label = comma(uo_i_total_perc)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Internet Usage (%)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_uoi

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_uoi.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# HH Items (%)
################################################################################

infographic_1_hh_items<- V4_T2.36
infographic_1_hh_items

infographic_1_hh_items_clean <- infographic_1_hh_items |>
  clean_names()

infographic_1_hh_items_clean <- infographic_1_hh_items_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("National|Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

#####################
# TV
#####################

# Find the top 10 subcounties (TV (%))

top_subcounty_tv <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(functional_tv)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_uoi)

# Find the bottom 10 subcounties (TV (%))

bottom_subcounty_uoi <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(functional_tv) |>
  slice(1:10) |>
  arrange(desc(functional_tv)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_tv)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_tv <- rbind(top_subcounty_tv, bottom_subcounty_tv)
View(merge_top_bottom_subcounty_tv)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_tv <- merge_top_bottom_subcounty_tv  |>
  ggplot(aes(x = reorder(county_sub, functional_tv), y = functional_tv, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = functional_tv+5, label = comma(functional_tv)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "TV (%)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_tv

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_tv.png",
       width = 12, height = 12, dpi = 300)

#####################
# Car (%)
#####################

# Find the top 10 subcounties (Car (%))

top_subcounty_car <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(car)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_car)

# Find the bottom 10 subcounties (Car (%))

bottom_subcounty_car <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(car) |>
  slice(1:10) |>
  arrange(desc(car)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_car)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_car <- rbind(top_subcounty_car, bottom_subcounty_car)
View(merge_top_bottom_subcounty_car)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_car <- merge_top_bottom_subcounty_car  |>
  ggplot(aes(x = reorder(county_sub, car), y = car, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = car+2.5, label = comma(car)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Car (%)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_car

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_car.png",
       width = 12, height = 12, dpi = 300)

#####################
# Radio (%)
#####################

# Find the top 10 subcounties (Radio (%))

top_subcounty_radio <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(stand_alone_radio)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_radio)

# Find the bottom 10 subcounties (Radio (%))

bottom_subcounty_radio <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(stand_alone_radio) |>
  slice(1:10) |>
  arrange(desc(stand_alone_radio)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_radio)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_radio <- rbind(top_subcounty_radio, bottom_subcounty_radio)
View(merge_top_bottom_subcounty_radio)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_radio <- merge_top_bottom_subcounty_radio |>
  ggplot(aes(x = reorder(county_sub, stand_alone_radio), y = stand_alone_radio, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = stand_alone_radio+7.5, label = comma(stand_alone_radio)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Radio (%)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28),
        axis.title.y =element_text(size = 28),
        axis.text.x =element_text(size = 24, color = "black"),
        axis.text.y =element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text(family = "URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "")

subcounty_plot_radio

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_radio.png",
       width = 12, height = 12, dpi = 300)



