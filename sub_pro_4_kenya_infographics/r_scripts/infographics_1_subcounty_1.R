# Subcounty Analysis - Infographics 1
# Sex and Sex Ratio

# Load libraries

library(patchwork)
library(tidyverse)
library(ggbreak)
library(rKenyaCensus)
library(janitor)

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
  filter(!str_detect(sub_county, regex("Park|Forest"))) |>
  mutate(county = str_remove(county, regex("\\bCounty\\b"))) |>
  mutate(across(where(is.character),str_squish))

# Calculate the male:female ratio per 100
infographic_1_pop_clean_ratio <- infographic_1_pop_clean |>
  mutate(m_f_ratio = male/female,
         m_f_ratio_100 = round(m_f_ratio*100, 0))

# Find the top 10 subcounties
top_subcounty <- infographic_1_pop_clean_ratio |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(desc(m_f_ratio_100)) |>
  slice(1:10) |>
  mutate(bar_color = "Top")

View(top_subcounty)

# Find the bottom 10 subcounties
bottom_subcounty <- infographic_1_pop_clean_ratio |>
  unite(col = "county_sub", c("sub_county", "county"), sep = ", ", remove = TRUE) |>
  arrange(m_f_ratio_100) |>
  slice(1:10) |>
  arrange(desc(m_f_ratio_100)) |>
  mutate(bar_color = "Bottom")
  
View(bottom_subcounty)




merge_top_bottom_subcounty <- rbind(top_subcounty, bottom_subcounty)

classification_colors <- c(Top = "#000000", Bottom = "#BB0000")

subcounty_plot_sex_ratio <- merge_top_bottom_subcounty  |>
  ggplot(aes(x = reorder(county_sub, m_f_ratio_100), y = m_f_ratio_100, fill = bar_color)) + 
  geom_col(width = 0.95) + 
  coord_flip() + 
  geom_text(aes(x = county_sub, y = 0, label = county_sub), 
            color = "white", 
            fontface = "bold",
            size = 8,
            hjust = 0) +
  geom_text(aes(x = county_sub, y = m_f_ratio_100-4, label = m_f_ratio_100), 
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
        axis.text.x =element_text(size = 24),
        axis.text.y =element_blank(),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2"),
        legend.position = "") + 
  geom_hline(yintercept = 98, linetype="dashed", color = "#006600", size=1)

subcounty_plot_sex_ratio

ggsave("sub_pro_4_kenya_infographics/images/infographics_1_subcounty/top_bottom_subcounty_sex_ratio.png", 
       width = 12, height = 12, dpi = 300)
