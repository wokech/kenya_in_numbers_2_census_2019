# Subcounty Analysis - Infographics 1

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
# Desktop, Laptop, or Tablet Usage (%)
################################################################################

infographic_1_uodlt <- V4_T2.33
infographic_1_uodlt

infographic_1_uodlt_clean <- infographic_1_uodlt |>
  clean_names()

infographic_1_uodlt_clean <- infographic_1_uodlt_clean |>
  filter(!str_detect(sub_county, regex("KENYA|URBAN|RURAL", ignore_case = TRUE))) |>
  filter(admin_area != "County") |>
  filter(!str_detect(sub_county, regex("Forest|Park", ignore_case = TRUE))) |>
  mutate(across(where(is.character),str_squish)) |> 
  mutate(county = tools::toTitleCase(tolower(county))) |> 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

# Find the top 10 subcounties (Desktop, Laptop, or Tablet Usage (%))

top_subcounty_uodlt <- infographic_1_uodlt_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(uo_dlt_total_perc)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_uodlt)

# Find the bottom 10 subcounties (Desktop, Laptop, or Tablet Usage (%))

bottom_subcounty_uodlt <- infographic_1_uodlt_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(uo_dlt_total_perc) |>
  slice(1:10) |>
  arrange(desc(uo_dlt_total_perc)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_uodlt)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_uodlt <- rbind(top_subcounty_uodlt, bottom_subcounty_uodlt)
View(merge_top_bottom_subcounty_uodlt)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_uodlt <- merge_top_bottom_subcounty_uodlt  |>
  ggplot(aes(x = reorder(county_sub, uo_dlt_total_perc), y = uo_dlt_total_perc, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = uo_dlt_total_perc+5, label = comma(uo_dlt_total_perc)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Desktop, Laptop, or Tablet Usage (%)", 
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

subcounty_plot_uodlt

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_subcounty/top_bottom_subcounty_uodlt.png",
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
  mutate(sub_county = tools::toTitleCase(tolower(sub_county))) |>
  mutate(county = recode(county, "Taita/Taveta" = "Taita Taveta"),
         county = recode(county, "Tharaka-Nithi" = "Tharaka Nithi"),
         county = recode(county, "Elgeyo/Marakwet" = "Elgeyo Marakwet"))


#####################
# DeskTop/Computer/Laptop/Tablet (%)
#####################

# Find the top 10 subcounties (DeskTop/Computer/Laptop/Tablet (%))

top_subcounty_dclt <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(desk_top_computer_laptop_tablet)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_dclt)

# Find the bottom 10 subcounties (DeskTop/Computer/Laptop/Tablet (%))

bottom_subcounty_dclt <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desk_top_computer_laptop_tablet) |>
  slice(1:10) |>
  arrange(desc(desk_top_computer_laptop_tablet)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_dclt)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_dclt <- rbind(top_subcounty_dclt, bottom_subcounty_dclt)
View(merge_top_bottom_subcounty_dclt)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_dclt <- merge_top_bottom_subcounty_dclt  |>
  ggplot(aes(x = reorder(county_sub, desk_top_computer_laptop_tablet), y = desk_top_computer_laptop_tablet, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = desk_top_computer_laptop_tablet+5, label = comma(desk_top_computer_laptop_tablet)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = " Household Ownership of a\nPersonal Computing Device (%)", 
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

subcounty_plot_dclt

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_subcounty/top_bottom_subcounty_dclt.png",
       width = 12, height = 12, dpi = 300)

#####################
# Bicycle (%)
#####################

# Find the top 10 subcounties (Bicycle (%))

top_subcounty_bicycle <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(bicycle)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_bicycle)

# Find the bottom 10 subcounties (Bicycle (%))

bottom_subcounty_bicycle <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(bicycle) |>
  slice(1:10) |>
  arrange(desc(bicycle)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_bicycle)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_bicycle <- rbind(top_subcounty_bicycle, bottom_subcounty_bicycle)
View(merge_top_bottom_subcounty_bicycle)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_bicycle <- merge_top_bottom_subcounty_bicycle  |>
  ggplot(aes(x = reorder(county_sub, bicycle), y = bicycle, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = bicycle+5, label = comma(bicycle)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Bicycle (%)", 
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

subcounty_plot_bicycle

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_subcounty/top_bottom_subcounty_bicycle.png",
       width = 12, height = 12, dpi = 300)

#####################
# Motor Cycle (%)
#####################

# Find the top 10 subcounties (Motor Cycle (%))

top_subcounty_motor_cycle <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(motor_cycle)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_motor_cycle)

# Find the bottom 10 subcounties (Motor Cycle (%))

bottom_subcounty_motor_cycle <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(motor_cycle) |>
  slice(1:10) |>
  arrange(desc(motor_cycle)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_motor_cycle)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_motor_cycle <- rbind(top_subcounty_motor_cycle, bottom_subcounty_motor_cycle)
View(merge_top_bottom_subcounty_motor_cycle)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_motor_cycle <- merge_top_bottom_subcounty_motor_cycle |>
  ggplot(aes(x = reorder(county_sub, motor_cycle), y = motor_cycle, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = motor_cycle+2.5, label = comma(motor_cycle)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Motor Cycle (%)", 
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

subcounty_plot_motor_cycle

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_subcounty/top_bottom_subcounty_motor_cycle.png",
       width = 12, height = 12, dpi = 300)



#####################
# Refrigerator (%)
#####################

# Find the top 10 subcounties (Refrigerator (%))

top_subcounty_refrigerator <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(refrigerator)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_refrigerator)

# Find the bottom 10 subcounties (Refrigerator (%))

bottom_subcounty_refrigerator <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(refrigerator) |>
  slice(1:10) |>
  arrange(desc(refrigerator)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_refrigerator)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_refrigerator <- rbind(top_subcounty_refrigerator, bottom_subcounty_refrigerator)
View(merge_top_bottom_subcounty_refrigerator)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_refrigerator <- merge_top_bottom_subcounty_refrigerator |>
  ggplot(aes(x = reorder(county_sub, refrigerator), y = refrigerator, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = refrigerator+5, label = comma(refrigerator)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Refrigerator (%)", 
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

subcounty_plot_refrigerator

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_subcounty/top_bottom_subcounty_refrigerator.png",
       width = 12, height = 12, dpi = 300)



#####################
# Truck / Lorry / Bus / Three-Wheeler Truck (%)
#####################

# Find the top 10 subcounties (Truck / Lorry / Bus / Three-Wheeler Truck (%))

top_subcounty_tlbt <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(truck_lorry_bus_three_wheelertruck)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_tlbt)

# Find the bottom 10 subcounties (Truck / Lorry / Bus / Three-Wheeler Truck (%))

bottom_subcounty_tlbt <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(truck_lorry_bus_three_wheelertruck) |>
  slice(1:10) |>
  arrange(desc(truck_lorry_bus_three_wheelertruck)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_tlbt)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_tlbt <- rbind(top_subcounty_tlbt, bottom_subcounty_tlbt)
View(merge_top_bottom_subcounty_tlbt)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_tlbt <- merge_top_bottom_subcounty_tlbt |>
  ggplot(aes(x = reorder(county_sub, truck_lorry_bus_three_wheelertruck), y = truck_lorry_bus_three_wheelertruck, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = truck_lorry_bus_three_wheelertruck+0.25, label = comma(truck_lorry_bus_three_wheelertruck)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Truck, Lorry, Bus, or Three-Wheeler Truck (%)", 
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

subcounty_plot_tlbt

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_subcounty/top_bottom_subcounty_tlbt.png",
       width = 12, height = 12, dpi = 300)



#####################
# Tuk Tuk (%)
#####################

# Find the top 10 subcounties (Tuk Tuk (%))

top_subcounty_tuk_tuk <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(tuk_tuk)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty_tuk_tuk)

# Find the bottom 10 subcounties (Tuk Tuk (%))

bottom_subcounty_tuk_tuk <- infographic_1_hh_items_clean |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(tuk_tuk) |>
  slice(1:10) |>
  arrange(desc(tuk_tuk)) |>
  mutate(bar_color = "Bottom")

View(bottom_subcounty_tuk_tuk)

# Merge the top and bottom subcounties

merge_top_bottom_subcounty_tuk_tuk <- rbind(top_subcounty_tuk_tuk, bottom_subcounty_tuk_tuk)
View(merge_top_bottom_subcounty_tuk_tuk)

# Plot the top and bottom subcounties

# Set up the classification colors
classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_tuk_tuk <- merge_top_bottom_subcounty_tuk_tuk |>
  ggplot(aes(x = reorder(county_sub, tuk_tuk), y = tuk_tuk, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = tuk_tuk+0.5, label = comma(tuk_tuk)), 
            color = "black", 
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = classification_colors) +
  theme_classic()+
  labs(x = "", 
       y = "Tuk Tuk (%)", 
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

subcounty_plot_tuk_tuk

ggsave("sub_pro_4_kenya_infographics/images/infographics_3_subcounty/top_bottom_subcounty_tuk_tuk.png",
       width = 12, height = 12, dpi = 300)
