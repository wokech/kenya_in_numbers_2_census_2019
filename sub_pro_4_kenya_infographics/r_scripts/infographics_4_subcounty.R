# Subcounty Analysis - Infographics 4

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


################################################################################
# 1) Total Piped Water to Compound (%)
################################################################################

infographic_4_piped_water <- V4_T2.15
infographic_4_piped_water

infographic_4_piped_water_clean <- infographic_4_piped_water |>
  clean_names()

infographic_4_piped_water_clean <- infographic_4_piped_water_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with Piped Water (%))

top_subcounty_piped_water <- infographic_4_piped_water_clean |>
  mutate(total_piped = pipedintodwelling + pipedtoyard_plot) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_piped) |>
  arrange(desc(total_piped)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_piped_water)

# Find the bottom 10 subcounties (Households with Piped Water (%))

bottom_subcounty_piped_water <- infographic_4_piped_water_clean |>
  mutate(total_piped = pipedintodwelling + pipedtoyard_plot) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_piped) |>
  arrange(total_piped) |>
  slice(1:10) |>
  arrange(desc(total_piped)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_piped_water)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_piped_water <- rbind(top_subcounty_piped_water, bottom_subcounty_piped_water)

View(merge_top_bottom_subcounty_piped_water)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_piped_water <- merge_top_bottom_subcounty_piped_water  |>
  ggplot(aes(x = reorder(county_sub, total_piped), y = total_piped, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = total_piped+5, label = comma(total_piped)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households with piped water to compound (%)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28, margin = margin(t = 20)),
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

subcounty_plot_piped_water

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_piped_water.png",
       width = 12, height = 12, dpi = 300)


################################################################################
# 2) Total Piped Water to Compound (%)
################################################################################

infographic_4_piped_water <- V4_T2.15
infographic_4_piped_water

infographic_4_piped_water_clean <- infographic_4_piped_water |>
  clean_names()

infographic_4_piped_water_clean <- infographic_4_piped_water_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with Piped Water (%))

top_subcounty_piped_water <- infographic_4_piped_water_clean |>
  mutate(total_piped = pipedintodwelling + pipedtoyard_plot) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_piped) |>
  arrange(desc(total_piped)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_piped_water)

# Find the bottom 10 subcounties (Households with Piped Water (%))

bottom_subcounty_piped_water <- infographic_4_piped_water_clean |>
  mutate(total_piped = pipedintodwelling + pipedtoyard_plot) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_piped) |>
  arrange(total_piped) |>
  slice(1:10) |>
  arrange(desc(total_piped)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_piped_water)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_piped_water <- rbind(top_subcounty_piped_water, bottom_subcounty_piped_water)

View(merge_top_bottom_subcounty_piped_water)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_piped_water <- merge_top_bottom_subcounty_piped_water  |>
  ggplot(aes(x = reorder(county_sub, total_piped), y = total_piped, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = total_piped+5, label = comma(total_piped)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households with piped water to compound (%)", 
       title = "",
       caption = "") +
  theme(axis.title.x =element_text(size = 28, margin = margin(t = 20)),
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

subcounty_plot_piped_water

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_piped_water.png",
       width = 12, height = 12, dpi = 300)
