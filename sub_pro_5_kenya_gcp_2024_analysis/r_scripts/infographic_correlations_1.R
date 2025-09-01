# KNBS Gross County Product Analysis 2024
# By @kenya.in.numbers
# From GCP 2024

#####################
#####PART A
#####################

# Load all the required packages and libraries required for accessing the census data

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
library(ggtext)

# Load the required data

# Internet Usage (2022)
mobile_phone_internet_use <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                   "datasets", "kenya_gcp_2024_tables",
                                                   "mobile_phone_internet_use.xlsx"))

# Poverty Estimates (2015 - 2022)
poverty_estimates <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                           "datasets", "kenya_gcp_2024_tables",
                                           "poverty_estimates.xlsx"))

# Share of the Gross County Product (5 yr avg, 2019 - 2023)
avg_share_gcp_2019_2023 <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                 "datasets", "kenya_gcp_2024_tables",
                                                 "avg_share_gcp_2019_2023.xlsx"))

# Clean the data

mobile_phone_internet_use <- mobile_phone_internet_use |>
  clean_names() |>
  filter(county != "NATIONAL") |>
  mutate(county = ifelse(county == "MURANG’A", "MURANG'A", county))

poverty_estimates <- poverty_estimates |>
  clean_names() |>
  filter(!residence_county %in% c("NATIONAL", "RURAL", "URBAN")) |>
  select(residence_county, x2022_percent)

avg_share_gcp_2019_2023 <- avg_share_gcp_2019_2023 |>
  clean_names()

avg_share_gcp_2019_2023_select <- avg_share_gcp_2019_2023 |>
  select(c(county_number, county, x5_year_avg)) |>
  filter(county != "TOTAL")

unique(avg_share_gcp_2019_2023_select$county)
avg_share_gcp_2019_2023_select$county <- gsub("/", " ", avg_share_gcp_2019_2023_select$county)
avg_share_gcp_2019_2023_select$county <- gsub("-", " ", avg_share_gcp_2019_2023_select$county)
avg_share_gcp_2019_2023_select <- avg_share_gcp_2019_2023_select |> 
  mutate(county = tools::toTitleCase(tolower(county)))
unique(avg_share_gcp_2019_2023_select$county)

# Rename Murang'a
avg_share_gcp_2019_2023_select <- avg_share_gcp_2019_2023_select |>
  mutate(county = recode(county, "Murang’a" = "Murang'a"))

merged_1 <- poverty_estimates |>
  left_join(mobile_phone_internet_use, by = c("residence_county" = "county"))

# Fix the county names

merged_1$residence_county <- gsub("/", " ", merged_1$residence_county)
merged_1$residence_county <- gsub("-", " ", merged_1$residence_county)

### Convert the county names to title case
merged_1 <- merged_1 |> 
  mutate(residence_county = tools::toTitleCase(tolower(residence_county)))

merged_all <- merged_1 |>
  left_join(avg_share_gcp_2019_2023_select, by = c("residence_county" = "county"))

################################################################################
# POVERTY CORRELATIONS
################################################################################

# Households that report to using the internet (KDHS, %)

# Thresholds
internet_thresh <- median(merged_all$used_internet_percent)
poverty_thresh <- median(merged_all$x2022_percent)

ggplot(merged_all, aes(x = used_internet_percent, y = x2022_percent)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Internet Usage (%)", y = "Poverty Incidence (%)", title = "") +
  annotate("rect", xmin = internet_thresh, xmax = Inf,
           ymin = -Inf, ymax = poverty_thresh, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = poverty_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = internet_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/poverty_internet.png", width = 12, height = 12, dpi = 300)

# Households with access to a television (KDHS, %) 

# Thresholds
television_thresh <- median(merged_all$television_percent)
poverty_thresh <- median(merged_all$x2022_percent)

ggplot(merged_all, aes(x = television_percent, y = x2022_percent)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Television Ownership (%)", y = "Poverty Incidence (%)", title = "") +
  annotate("rect", xmin = television_thresh, xmax = Inf,
           ymin = -Inf, ymax = poverty_thresh, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = poverty_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = television_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/poverty_television.png", width = 12, height = 12, dpi = 300)

# Households with access to a computer (KDHS, %) 

# Thresholds
computer_thresh <- median(merged_all$computer_percent)
poverty_thresh <- median(merged_all$x2022_percent)

ggplot(merged_all, aes(x = computer_percent, y = x2022_percent)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Computer Ownership (%)", y = "Poverty Incidence (%)", title = "") +
  annotate("rect", xmin = computer_thresh, xmax = Inf,
           ymin = -Inf, ymax = poverty_thresh, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = poverty_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = computer_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/poverty_computer.png", width = 12, height = 12, dpi = 300)

# Households having at least one member owning a mobile phone (KDHS, %) 

# Thresholds
mobile_thresh <- median(merged_all$mobile_phone_percent)
poverty_thresh <- median(merged_all$x2022_percent)

ggplot(merged_all, aes(x = mobile_phone_percent, y = x2022_percent)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Mobile Phone Ownership (%)", y = "Poverty Incidence (%)", title = "") +
  annotate("rect", xmin = mobile_thresh, xmax = Inf,
           ymin = -Inf, ymax = poverty_thresh, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = poverty_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = mobile_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/poverty_mobile.png", width = 12, height = 12, dpi = 300)

################################################################################
# GCP CORRELATIONS
################################################################################


# Households that report to using the internet (KDHS, %)

# Thresholds
internet_thresh <- median(merged_all$used_internet_percent)
gcp_thresh <- median(merged_all$x5_year_avg)

ggplot(merged_all, aes(x = used_internet_percent, y = x5_year_avg)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Internet Usage (%)", y = "GCP Share (%)", title = "") +
  annotate("rect", xmin = internet_thresh, xmax = Inf,
           ymin = gcp_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = gcp_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = internet_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/gcp_internet.png", width = 12, height = 12, dpi = 300)

# Households with access to a television (KDHS, %) 

# Thresholds
television_thresh <- median(merged_all$television_percent)
gcp_thresh <- median(merged_all$x5_year_avg)

ggplot(merged_all, aes(x = television_percent, y = x5_year_avg)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Television Ownership (%)", y = "GCP Share (%)", title = "") +
  annotate("rect", xmin = television_thresh, xmax = Inf,
           ymin = gcp_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = gcp_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = television_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/gcp_television.png", width = 12, height = 12, dpi = 300)

# Households with access to a computer (KDHS, %) 

# Thresholds
computer_thresh <- median(merged_all$computer_percent)
gcp_thresh <- median(merged_all$x5_year_avg)

ggplot(merged_all, aes(x = computer_percent, y = x5_year_avg)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Computer Ownership (%)", y = "GCP Share (%)", title = "") +
  annotate("rect", xmin = computer_thresh, xmax = Inf,
           ymin = gcp_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = gcp_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = computer_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/gcp_computer.png", width = 12, height = 12, dpi = 300)

# Households having at least one member owning a mobile phone (KDHS, %) 

# Thresholds
mobile_thresh <- median(merged_all$mobile_phone_percent)
gcp_thresh <- median(merged_all$x5_year_avg)

ggplot(merged_all, aes(x = mobile_phone_percent, y = x5_year_avg)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Mobile Phone Ownership (%)", y = "GCP Share (%)", title = "") +
  annotate("rect", xmin = mobile_thresh, xmax = Inf,
           ymin = gcp_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = gcp_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = mobile_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

# ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/gcp_mobile.png", width = 12, height = 12, dpi = 300)

################################################################################
# POVERTY (%) VS GCP (%)
################################################################################

# With Nairobi

# Thresholds
gcp_thresh <- median(merged_all$x5_year_avg)
poverty_thresh <- median(merged_all$x2022_percent)

ggplot(merged_all, aes(x = x2022_percent, y = x5_year_avg)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Poverty Incidence (%)", y = "GCP Share (%)", title = "") +
  annotate("rect", xmin = poverty_thresh, xmax = Inf,
           ymin = gcp_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = gcp_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = poverty_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/gcp_poverty_with_nai.png", width = 12, height = 12, dpi = 300)

# Without Nairobi

merged_all_no_nai <- merged_all |>
  filter(residence_county != "Nairobi City")

# Thresholds
gcp_thresh <- median(merged_all_no_nai$x5_year_avg)
poverty_thresh <- median(merged_all_no_nai$x2022_percent)

ggplot(merged_all_no_nai, aes(x = x2022_percent, y = x5_year_avg)) +
  geom_jitter(color = "brown4", size = 4) +
  geom_text_repel(aes(label = residence_county), color = "brown4", size = 8) +
  labs(x = "Poverty Incidence (%)", y = "GCP Share (%)", title = "") +
  annotate("rect", xmin = poverty_thresh, xmax = Inf,
           ymin = gcp_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = gcp_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = poverty_thresh, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 32),
        axis.title.y =element_text(size = 32, angle = 90),
        axis.text.x =element_text(size = 24),
        axis.text.y =element_text(size = 24),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(5, "pt"),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(5, "pt"),
        plot.title = element_markdown(family = "Helvetica",size = 36, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/infographic_correlations_1/gcp_poverty_without_nai.png", width = 12, height = 12, dpi = 300)
