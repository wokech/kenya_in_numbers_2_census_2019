# Kenya Instagram Hashtag Analysis 2
# By @kenya.in.numbers

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

# Insta Hashtags (July 2025)
kenya_insta_hashtags <- read_excel(here::here("sub_pro_4_kenya_infographics", 
                                              "datasets", "kenya_insta_hashtags_1.xlsx"))

kenya_insta_hashtags <- kenya_insta_hashtags |>
  clean_names()

################################################################################
# WITH NAIROBI, MOMBASA, AND LAMU
################################################################################

# Poverty (%) and Normalized Hashtags (%)

# Thresholds
insta_thresh <- median(kenya_insta_hashtags$insta_hashtag_population_percent_july_2025)
poverty_thresh <- median(kenya_insta_hashtags$poverty_estimates_percent_2022)

ggplot(kenya_insta_hashtags, aes(x = poverty_estimates_percent_2022, y = insta_hashtag_population_percent_july_2025)) +
  geom_jitter(color = "brown4", size = 5) +
  geom_text_repel(data = subset(kenya_insta_hashtags, 
                                poverty_estimates_percent_2022 > poverty_thresh & insta_hashtag_population_percent_july_2025 > insta_thresh),
                  aes(label = hashtag), color = "black", size = 8) +
  geom_text_repel(data = subset(kenya_insta_hashtags, 
                                insta_hashtag_population_percent_july_2025 > 100),
                  aes(label = hashtag), color = "black", size = 8) +
  labs(x = "Poverty Incidence (%)", y = "Hashtags/Population (%)", title = "") +
  annotate("rect", xmin = poverty_thresh, xmax = Inf,
           ymin = insta_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = insta_thresh, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/poverty_hashtags.png", width = 12, height = 12, dpi = 300)


# Share of contribution to GDP (%) and Normalized Hashtags (%)

# Thresholds
insta_thresh <- median(kenya_insta_hashtags$insta_hashtag_population_percent_july_2025)
gcp_thresh <- median(kenya_insta_hashtags$gcp_share_percent_2019_2023)

ggplot(kenya_insta_hashtags, aes(x = gcp_share_percent_2019_2023, y = insta_hashtag_population_percent_july_2025)) +
  geom_jitter(color = "brown4", size = 5) +
  geom_text_repel(data = subset(kenya_insta_hashtags, 
                                gcp_share_percent_2019_2023 > 3 | insta_hashtag_population_percent_july_2025 > 9),
                  aes(label = hashtag), color = "black", size = 8) +
  labs(x = "Share of contribution to GDP (%)", y = "Hashtags/Population (%)", title = "") +
  annotate("rect", xmin = gcp_thresh, xmax = Inf,
           ymin = insta_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = insta_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = gcp_thresh, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/gcp_hashtags.png", width = 12, height = 12, dpi = 300)


# Food/Accomodation and Normalized Hashtags (%)

# Thresholds
insta_thresh <- median(kenya_insta_hashtags$insta_hashtag_population_percent_july_2025)
food_acc_thresh <- median(kenya_insta_hashtags$accommodation_food_service_activities_millions_kshs_2023)

ggplot(kenya_insta_hashtags, aes(x = accommodation_food_service_activities_millions_kshs_2023, y = insta_hashtag_population_percent_july_2025)) +
  geom_jitter(color = "brown4", size = 5) +
  geom_text_repel(data = subset(kenya_insta_hashtags, 
                                accommodation_food_service_activities_millions_kshs_2023 > 3400 | insta_hashtag_population_percent_july_2025 > 10),
                  aes(label = hashtag), color = "black", size = 8) +
  labs(x = "Contribution of Food and Accommodation\nServices (2023) to the GDP (Millions of Kshs)", y = "Hashtags/Population (%)", title = "") +
  annotate("rect", xmin = food_acc_thresh, xmax = Inf,
           ymin = insta_thresh, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = insta_thresh, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = food_acc_thresh, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/food_accommodation_hashtags.png", width = 12, height = 12, dpi = 300)

################################################################################
# WITHOUT NAIROBI, MOMBASA, AND LAMU
################################################################################

kenya_insta_hashtags_no_nai_mom_lamu <- kenya_insta_hashtags |>
  filter(insta_hashtag_population_percent_july_2025 < 100)

# Poverty (%) and Normalized Hashtags (%) - w/o Nai/Mom/Lamu

# Thresholds
insta_thresh_no_nai_mom_lamu <- median(kenya_insta_hashtags_no_nai_mom_lamu$insta_hashtag_population_percent_july_2025)
poverty_thresh_no_nai_mom_lamu <- median(kenya_insta_hashtags_no_nai_mom_lamu$poverty_estimates_percent_2022)

ggplot(kenya_insta_hashtags_no_nai_mom_lamu, aes(x = poverty_estimates_percent_2022, y = insta_hashtag_population_percent_july_2025)) +
  geom_jitter(color = "brown4", size = 5) +
  geom_text_repel(data = subset(kenya_insta_hashtags_no_nai_mom_lamu, 
                                poverty_estimates_percent_2022 > poverty_thresh_no_nai_mom_lamu & insta_hashtag_population_percent_july_2025 > insta_thresh_no_nai_mom_lamu),
                  aes(label = hashtag), color = "black", size = 8) +
  labs(x = "Poverty Incidence (%)", y = "Hashtags/Population (%)", title = "") +
  annotate("rect", xmin = poverty_thresh_no_nai_mom_lamu, xmax = Inf,
           ymin = insta_thresh_no_nai_mom_lamu, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = insta_thresh_no_nai_mom_lamu, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = poverty_thresh_no_nai_mom_lamu, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/poverty_hashtags_no_nai_mom_lamu.png", width = 12, height = 12, dpi = 300)


# Share of contribution to GDP (%) and Normalized Hashtags (%) - w/o Nai/Mom/Lamu

# Thresholds
insta_thresh_no_nai_mom_lamu <- median(kenya_insta_hashtags_no_nai_mom_lamu$insta_hashtag_population_percent_july_2025)
gcp_thresh_no_nai_mom_lamu <- median(kenya_insta_hashtags_no_nai_mom_lamu$gcp_share_percent_2019_2023)

ggplot(kenya_insta_hashtags_no_nai_mom_lamu, aes(x = gcp_share_percent_2019_2023, y = insta_hashtag_population_percent_july_2025)) +
  geom_jitter(color = "brown4", size = 5) +
  geom_text_repel(data = subset(kenya_insta_hashtags_no_nai_mom_lamu, 
                                gcp_share_percent_2019_2023 > gcp_thresh_no_nai_mom_lamu & insta_hashtag_population_percent_july_2025 > insta_thresh_no_nai_mom_lamu),
                  aes(label = hashtag), color = "black", size = 8) +
  labs(x = "Share of contribution to GDP (%)", y = "Hashtags/Population (%)", title = "") +
  annotate("rect", xmin = gcp_thresh_no_nai_mom_lamu, xmax = Inf,
           ymin = insta_thresh_no_nai_mom_lamu, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = insta_thresh_no_nai_mom_lamu, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = gcp_thresh_no_nai_mom_lamu, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/gcp_hashtags_no_nai_mom_lamu.png", width = 12, height = 12, dpi = 300)


# Food/Accomodation and Normalized Hashtags (%) - w/o Nai/Mom/Lamu

# Thresholds
insta_thresh_no_nai_mom_lamu <- median(kenya_insta_hashtags_no_nai_mom_lamu$insta_hashtag_population_percent_july_2025)
food_acc_thresh_no_nai_mom_lamu <- median(kenya_insta_hashtags_no_nai_mom_lamu$accommodation_food_service_activities_millions_kshs_2023)

ggplot(kenya_insta_hashtags_no_nai_mom_lamu, aes(x = accommodation_food_service_activities_millions_kshs_2023, y = insta_hashtag_population_percent_july_2025)) +
  geom_jitter(color = "brown4", size = 5) +
  geom_text_repel(data = subset(kenya_insta_hashtags_no_nai_mom_lamu, 
                                accommodation_food_service_activities_millions_kshs_2023 > food_acc_thresh_no_nai_mom_lamu & insta_hashtag_population_percent_july_2025 > insta_thresh_no_nai_mom_lamu),
                  aes(label = hashtag), color = "black", size = 8) +
  labs(x = "Contribution of Food and Accommodation\nServices (2023) to the GDP (Millions of Kshs)", y = "Hashtags/Population (%)", title = "") +
  annotate("rect", xmin = food_acc_thresh_no_nai_mom_lamu, xmax = Inf,
           ymin = insta_thresh_no_nai_mom_lamu, ymax = Inf, alpha = 0.2, fill = "pink") +
  geom_hline(yintercept = insta_thresh_no_nai_mom_lamu, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = food_acc_thresh_no_nai_mom_lamu, linetype = "dashed", color = "gray") +
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

ggsave("sub_pro_4_kenya_infographics/images/hashtag_plots/food_accommodation_hashtags_no_nai_mom_lamu.png", width = 12, height = 12, dpi = 300)
