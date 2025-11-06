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
# 2) Open Bush (%)
################################################################################

infographic_4_open_bush <- V4_T2.16
infographic_4_open_bush

infographic_4_open_bush_clean <- infographic_4_open_bush |>
  clean_names()

infographic_4_open_bush_clean <- infographic_4_open_bush_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with Piped Water (%))

top_subcounty_open_bush <- infographic_4_open_bush_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, open_bush) |>
  arrange(desc(open_bush)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_open_bush)

# Find the bottom 10 subcounties (Households with Piped Water (%))

bottom_subcounty_open_bush <- infographic_4_open_bush_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, open_bush) |>
  arrange(open_bush) |>
  slice(1:10) |>
  arrange(desc(open_bush)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_open_bush)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_open_bush <- rbind(top_subcounty_open_bush, bottom_subcounty_open_bush)

View(merge_top_bottom_subcounty_open_bush)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_open_bush <- merge_top_bottom_subcounty_open_bush  |>
  ggplot(aes(x = reorder(county_sub, open_bush), y = open_bush, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = open_bush+5, label = comma(open_bush)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households practising open defecation (%)", 
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

subcounty_plot_open_bush

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_open_bush.png",
       width = 12, height = 12, dpi = 300)


################################################################################
# 3) Total Flushing (%)
################################################################################

infographic_4_total_flush <- V4_T2.16
infographic_4_total_flush

infographic_4_total_flush_clean <- infographic_4_total_flush |>
  clean_names()

infographic_4_total_flush_clean <- infographic_4_total_flush_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with flushing toilets (septic/sewer) (%))

top_subcounty_total_flush <- infographic_4_total_flush_clean |>
  mutate(total_flush = main_sewer + septic_tank + bio_septic_tank_biodigester) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_flush) |>
  arrange(desc(total_flush)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_total_flush)

# Find the bottom 10 subcounties (Households with flushing toilets (septic/sewer) (%))

bottom_subcounty_total_flush <- infographic_4_total_flush_clean |>
  mutate(total_flush = main_sewer + septic_tank + bio_septic_tank_biodigester) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_flush) |>
  arrange(total_flush) |>
  slice(1:10) |>
  arrange(desc(total_flush)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_total_flush)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_total_flush <- rbind(top_subcounty_total_flush, bottom_subcounty_total_flush)

View(merge_top_bottom_subcounty_total_flush)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_total_flush <- merge_top_bottom_subcounty_total_flush  |>
  ggplot(aes(x = reorder(county_sub, total_flush), y = total_flush, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = total_flush+5, label = comma(total_flush)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households with flushing toilets - septic/sewer (%)", 
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

subcounty_plot_total_flush

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_total_flush.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# 4) Gas Usage (%)
################################################################################

infographic_4_total_gas <- V4_T2.18
infographic_4_total_gas

infographic_4_total_gas_clean <- infographic_4_total_gas |>
  clean_names()

infographic_4_total_gas_clean <- infographic_4_total_gas_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with flushing toilets (septic/sewer) (%))

top_subcounty_total_gas <- infographic_4_total_gas_clean |>
  mutate(total_gas = gas_lpg + biogas) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_gas) |>
  arrange(desc(total_gas)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_total_gas)

# Find the bottom 10 subcounties (Households with flushing toilets (septic/sewer) (%))

bottom_subcounty_total_gas <- infographic_4_total_gas_clean |>
  mutate(total_gas = gas_lpg + biogas) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_gas) |>
  arrange(total_gas) |>
  slice(1:10) |>
  arrange(desc(total_gas)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_total_gas)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_total_gas <- rbind(top_subcounty_total_gas, bottom_subcounty_total_gas)

View(merge_top_bottom_subcounty_total_gas)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_total_gas <- merge_top_bottom_subcounty_total_gas  |>
  ggplot(aes(x = reorder(county_sub, total_gas), y = total_gas, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = total_gas+5, label = comma(total_gas)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households with flushing toilets - septic/sewer (%)", 
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

subcounty_plot_total_gas

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_total_gas.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# 5) Wood Fuel Usage (%)
################################################################################

infographic_4_total_wood <- V4_T2.18
infographic_4_total_wood

infographic_4_total_wood_clean <- infographic_4_total_wood |>
  clean_names()

infographic_4_total_wood_clean <- infographic_4_total_wood_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with flushing toilets (septic/sewer) (%))

top_subcounty_total_wood <- infographic_4_total_wood_clean |>
  mutate(total_wood = firewood + charcoal) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_wood) |>
  arrange(desc(total_wood)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_total_wood)

# Find the bottom 10 subcounties (Households with flushing toilets (septic/sewer) (%))

bottom_subcounty_total_wood <- infographic_4_total_wood_clean |>
  mutate(total_wood = firewood + charcoal) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_wood) |>
  arrange(total_wood) |>
  slice(1:10) |>
  arrange(desc(total_wood)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_total_wood)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_total_wood <- rbind(top_subcounty_total_wood, bottom_subcounty_total_wood)

View(merge_top_bottom_subcounty_total_wood)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_total_wood <- merge_top_bottom_subcounty_total_wood  |>
  ggplot(aes(x = reorder(county_sub, total_wood), y = total_wood, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = total_wood+5, label = comma(total_wood)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households cooking with firewood and charcoal (%)", 
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

subcounty_plot_total_wood

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_total_wood.png",
       width = 12, height = 12, dpi = 300)

################################################################################
# 6) Mains Electricity Usage for Lighting (%)
################################################################################

infographic_4_mains_electricity <- V4_T2.19
infographic_4_mains_electricity

infographic_4_mains_electricity_clean <- infographic_4_mains_electricity |>
  clean_names()

infographic_4_mains_electricity_clean <- infographic_4_mains_electricity_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with flushing toilets (septic/sewer) (%))

top_subcounty_mains_electricity <- infographic_4_mains_electricity_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, mains_electricity) |>
  arrange(desc(mains_electricity)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_mains_electricity)

# Find the bottom 10 subcounties (Households with flushing toilets (septic/sewer) (%))

bottom_subcounty_mains_electricity <- infographic_4_mains_electricity_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, mains_electricity) |>
  arrange(mains_electricity) |>
  slice(1:10) |>
  arrange(desc(mains_electricity)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_mains_electricity)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_mains_electricity <- rbind(top_subcounty_mains_electricity, bottom_subcounty_mains_electricity)

View(merge_top_bottom_subcounty_mains_electricity)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_mains_electricity <- merge_top_bottom_subcounty_mains_electricity  |>
  ggplot(aes(x = reorder(county_sub, mains_electricity), y = mains_electricity, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = mains_electricity+5, label = comma(mains_electricity)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households cooking with firewood and charcoal (%)", 
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

subcounty_plot_mains_electricity

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_mains_electricity.png",
       width = 12, height = 12, dpi = 300)


################################################################################
# 7) Solar Usage for Lighting (%)
################################################################################

infographic_4_solar <- V4_T2.19
infographic_4_solar

infographic_4_solar_clean <- infographic_4_solar |>
  clean_names()

infographic_4_solar_clean <- infographic_4_solar_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with flushing toilets (septic/sewer) (%))

top_subcounty_solar <- infographic_4_solar_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, solar) |>
  arrange(desc(solar)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_solar)

# Find the bottom 10 subcounties (Households with flushing toilets (septic/sewer) (%))

bottom_subcounty_solar <- infographic_4_solar_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, solar) |>
  arrange(solar) |>
  slice(1:10) |>
  arrange(desc(solar)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_solar)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_solar <- rbind(top_subcounty_solar, bottom_subcounty_solar)

View(merge_top_bottom_subcounty_solar)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_solar <- merge_top_bottom_subcounty_solar  |>
  ggplot(aes(x = reorder(county_sub, solar), y = solar, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = solar+5, label = comma(solar)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households lighting with solar (%)", 
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

subcounty_plot_solar

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_solar.png",
       width = 12, height = 12, dpi = 300)


################################################################################
# 8) Torch Usage for Lighting (%)
################################################################################

infographic_4_torch <- V4_T2.19
infographic_4_torch

infographic_4_torch_clean <- infographic_4_torch |>
  clean_names()

infographic_4_torch_clean <- infographic_4_torch_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with flushing toilets (septic/sewer) (%))

top_subcounty_torch <- infographic_4_torch_clean |>
  mutate(total_torch = torch_spotlight_solar_charged + torch_spotlight_dry_cells) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_torch) |>
  arrange(desc(total_torch)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_torch)

# Find the bottom 10 subcounties (Households with flushing toilets (septic/sewer) (%))

bottom_subcounty_torch <- infographic_4_torch_clean |>
  mutate(total_torch = torch_spotlight_solar_charged + torch_spotlight_dry_cells) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_torch) |>
  arrange(total_torch) |>
  slice(1:10) |>
  arrange(desc(total_torch)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_torch)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_torch <- rbind(top_subcounty_torch, bottom_subcounty_torch)

View(merge_top_bottom_subcounty_torch)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_torch <- merge_top_bottom_subcounty_torch  |>
  ggplot(aes(x = reorder(county_sub, total_torch), y = total_torch, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = total_torch+5, label = comma(total_torch)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households lighting with torch (%)", 
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

subcounty_plot_torch

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_torch.png",
       width = 12, height = 12, dpi = 300)


################################################################################
# 9) Paraffin Usage for Lighting (%)
################################################################################

infographic_4_paraffin <- V4_T2.19
infographic_4_paraffin

infographic_4_paraffin_clean <- infographic_4_paraffin |>
  clean_names()

infographic_4_paraffin_clean <- infographic_4_paraffin_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Households with flushing toilets (septic/sewer) (%))

top_subcounty_paraffin <- infographic_4_paraffin_clean |>
  mutate(total_paraffin = paraffin_lantern + paraffin_pressure_lamp + paraffin_tin_lamp) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_paraffin) |>
  arrange(desc(total_paraffin)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_paraffin)

# Find the bottom 10 subcounties (Households with flushing toilets (septic/sewer) (%))

bottom_subcounty_paraffin <- infographic_4_paraffin_clean |>
  mutate(total_paraffin = paraffin_lantern + paraffin_pressure_lamp + paraffin_tin_lamp) |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  select(county_sub, total_paraffin) |>
  arrange(total_paraffin) |>
  slice(1:10) |>
  arrange(desc(total_paraffin)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_paraffin)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_paraffin <- rbind(top_subcounty_paraffin, bottom_subcounty_paraffin)

View(merge_top_bottom_subcounty_paraffin)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_paraffin <- merge_top_bottom_subcounty_paraffin  |>
  ggplot(aes(x = reorder(county_sub, total_paraffin), y = total_paraffin, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = total_paraffin+5, label = comma(total_paraffin)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Households lighting with paraffin (%)", 
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

subcounty_plot_paraffin

ggsave("sub_pro_4_kenya_infographics/images/infographics_4_subcounty/top_bottom_subcounty_paraffin.png",
       width = 12, height = 12, dpi = 300)

