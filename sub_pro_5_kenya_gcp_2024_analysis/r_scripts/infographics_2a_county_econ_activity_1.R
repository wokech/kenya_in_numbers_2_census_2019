# # Economic Activity per County (2023 Data)
# By @kenya.in.numbers
# Data: Kenya GCP 2024

# 1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
#install.packages("ggpmisc")
#library(ggpmisc) #ggplot2 extension
#webshot::install_phantomjs()
library(knitr)
library(kableExtra)
#install.packages("treemapify")
library(treemapify)
library(scales)
library(readxl)

# Economic Activity by County (2023)
gcp_econ_activity_2023 <- read_excel(here::here("sub_pro_5_kenya_gcp_2024_analysis", 
                                                "datasets", "kenya_gcp_2024_tables",
                                                "gcp_econ_activity_2023.xlsx"))

# 2) Wrangle the Data

gcp_econ_activity_2023 <- gcp_econ_activity_2023 |>
  clean_names()

gcp_econ_activity_2023_select <- gcp_econ_activity_2023 |>
  select(-c(financial_services_indirectly_measured, gcp))

unique(gcp_econ_activity_2023_select$county)
gcp_econ_activity_2023_select$county <- gsub("/", " ", gcp_econ_activity_2023_select$county)
gcp_econ_activity_2023_select$county <- gsub("-", " ", gcp_econ_activity_2023_select$county)
gcp_econ_activity_2023_select <- gcp_econ_activity_2023_select |> 
  mutate(county = tools::toTitleCase(tolower(county)))
unique(gcp_econ_activity_2023_select$county)

# Rename Murang'a and add totals
gcp_econ_activity_2023_select <- gcp_econ_activity_2023_select |>
  mutate(county = recode(county, "Murang’a" = "Murang'a")) |>
  adorn_totals("row")

# Rename county to "Total"
gcp_econ_activity_2023_select[48, 2] <- "Total"
  
# 3a) Visualize the data

gcp_econ_activity_2023_select_tidy <- gcp_econ_activity_2023_select |>
  pivot_longer(c(agriculture_forestry_fishing:other_service_activities), 
               names_to = "econ_activity", values_to = "contribution") |>
  mutate(econ_activity = ifelse(econ_activity == "agriculture_forestry_fishing", "Agriculture",
                         ifelse(econ_activity == "mining_quarrying", "Mining",
                         ifelse(econ_activity == "manufacturing", "Manufacturing",
                         ifelse(econ_activity == "electricity_supply", "Electricity Supply",
                         ifelse(econ_activity == "water_supply_waste_collection", "Water Supply & Waste Collection",
                         ifelse(econ_activity == "construction", "Construction",
                         ifelse(econ_activity == "wholesale_retail_trade_repair_of_motor_vehicles", "Wholesale, Retail, &\nMotor Vehicle Repair",
                         ifelse(econ_activity == "transport_storage", "Transport & Storage",
                         ifelse(econ_activity == "accommodation_food_service_activities", "Accommodation & Food Service",
                         ifelse(econ_activity == "information_communication", "ICT",
                         ifelse(econ_activity == "financial_insurance_activities", "Financial & Insurance Services",
                         ifelse(econ_activity == "real_estate_activities", "Real Estate",
                         ifelse(econ_activity == "professional_technical_services", "Professional & Technical Services",
                         ifelse(econ_activity == "administrative_support_services", "Administrative Support Services",
                         ifelse(econ_activity == "public_administration_defence", "Public Admin & Defence",
                         ifelse(econ_activity == "education", "Education",
                         ifelse(econ_activity == "human_health_social_work_activities", "Human Health & Social Work",
                         ifelse(econ_activity == "other_service_activities", "Other Services", 
                                econ_activity))))))))))))))))))) 

# 3b) Country-Level Data

# Kenya

kenya_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Total")

kenya_gcp_econ_activity_2023_top_5 <- kenya_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kenya_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/total_kenya.png", width = 12, height = 8, dpi = 300)


# 4) Arrange by County

# Mombasa

mombasa_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Mombasa")

mombasa_gcp_econ_activity_2023_top_5 <- mombasa_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(mombasa_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/mombasa.png", width = 12, height = 8, dpi = 300)


# Kwale

kwale_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kwale")

kwale_gcp_econ_activity_2023_top_5 <- kwale_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kwale_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kwale.png", width = 12, height = 8, dpi = 300)


# Kilifi

kilifi_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kilifi")

kilifi_gcp_econ_activity_2023_top_5 <- kilifi_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kilifi_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kilifi.png", width = 12, height = 8, dpi = 300)


# Tana River

tana_river_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Tana River")

tana_river_gcp_econ_activity_2023_top_5 <- tana_river_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(tana_river_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/tana_river.png", width = 12, height = 8, dpi = 300)


# Lamu

lamu_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Lamu")

lamu_gcp_econ_activity_2023_top_5 <- lamu_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(lamu_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/lamu.png", width = 12, height = 8, dpi = 300)


# Taita Taveta

taita_taveta_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Taita Taveta")

taita_taveta_gcp_econ_activity_2023_top_5 <- taita_taveta_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(taita_taveta_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/taita_taveta.png", width = 12, height = 8, dpi = 300)


# Garissa

garissa_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Garissa")

garissa_gcp_econ_activity_2023_top_5 <- garissa_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(garissa_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/garissa.png", width = 12, height = 8, dpi = 300)


# Wajir

wajir_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Wajir")

wajir_gcp_econ_activity_2023_top_5 <- wajir_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(wajir_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/wajir.png", width = 12, height = 8, dpi = 300)


# Mandera

mandera_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Mandera")

mandera_gcp_econ_activity_2023_top_5 <- mandera_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(mandera_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/mandera.png", width = 12, height = 8, dpi = 300)


# Marsabit

marsabit_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Marsabit")

marsabit_gcp_econ_activity_2023_top_5 <- marsabit_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(marsabit_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/marsabit.png", width = 12, height = 8, dpi = 300)


# Isiolo

isiolo_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Isiolo")

isiolo_gcp_econ_activity_2023_top_5 <- isiolo_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(isiolo_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/isiolo.png", width = 12, height = 8, dpi = 300)


# Meru

meru_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Meru")

meru_gcp_econ_activity_2023_top_5 <- meru_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(meru_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/meru.png", width = 12, height = 8, dpi = 300)



# Tharaka Nithi

tharaka_nithi_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Tharaka Nithi")

tharaka_nithi_gcp_econ_activity_2023_top_5 <- tharaka_nithi_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(tharaka_nithi_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/tharaka_nithi.png", width = 12, height = 8, dpi = 300)


# Embu

embu_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Embu")

embu_gcp_econ_activity_2023_top_5 <- embu_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(embu_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/embu.png", width = 12, height = 8, dpi = 300)


# Kitui

kitui_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kitui")

kitui_gcp_econ_activity_2023_top_5 <- kitui_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kitui_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kitui.png", width = 12, height = 8, dpi = 300)


# Machakos

machakos_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Machakos")

machakos_gcp_econ_activity_2023_top_5 <- machakos_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(machakos_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/machakos.png", width = 12, height = 8, dpi = 300)


# Makueni

makueni_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Makueni")

makueni_gcp_econ_activity_2023_top_5 <- makueni_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(makueni_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/makueni.png", width = 12, height = 8, dpi = 300)


# Nyandarua

nyandarua_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Nyandarua")

nyandarua_gcp_econ_activity_2023_top_5 <- nyandarua_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(nyandarua_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/nyandarua.png", width = 12, height = 8, dpi = 300)


# Nyeri

nyeri_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Nyeri")

nyeri_gcp_econ_activity_2023_top_5 <- nyeri_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(nyeri_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/nyeri.png", width = 12, height = 8, dpi = 300)


# Kirinyaga

kirinyaga_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kirinyaga")

kirinyaga_gcp_econ_activity_2023_top_5 <- kirinyaga_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kirinyaga_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kirinyaga.png", width = 12, height = 8, dpi = 300)


# Murang'a

muranga_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Murang'a")

muranga_gcp_econ_activity_2023_top_5 <- muranga_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(muranga_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/muranga.png", width = 12, height = 8, dpi = 300)



# Kiambu

kiambu_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kiambu")

kiambu_gcp_econ_activity_2023_top_5 <- kiambu_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kiambu_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kiambu.png", width = 12, height = 8, dpi = 300)



# Turkana

turkana_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Turkana")

turkana_gcp_econ_activity_2023_top_5 <- turkana_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(turkana_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/turkana.png", width = 12, height = 8, dpi = 300)


# West Pokot

west_pokot_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "West Pokot")

west_pokot_gcp_econ_activity_2023_top_5 <- west_pokot_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(west_pokot_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/west_pokot.png", width = 12, height = 8, dpi = 300)



# Samburu

samburu_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Samburu")

samburu_gcp_econ_activity_2023_top_5 <- samburu_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(samburu_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/samburu.png", width = 12, height = 8, dpi = 300)


# Trans Nzoia

trans_nzoia_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Trans Nzoia")

trans_nzoia_gcp_econ_activity_2023_top_5 <- trans_nzoia_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(trans_nzoia_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/trans_nzoia.png", width = 12, height = 8, dpi = 300)


# Uasin Gishu

uasin_gishu_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Uasin Gishu")

uasin_gishu_gcp_econ_activity_2023_top_5 <- uasin_gishu_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(uasin_gishu_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/uasin_gishu.png", width = 12, height = 8, dpi = 300)


# Elgeyo Marakwet

elgeyo_marakwet_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Elgeyo Marakwet")

elgeyo_marakwet_gcp_econ_activity_2023_top_5 <- elgeyo_marakwet_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(elgeyo_marakwet_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/elgeyo_marakwet.png", width = 12, height = 8, dpi = 300)



# Nandi

nandi_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Nandi")

nandi_gcp_econ_activity_2023_top_5 <- nandi_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(nandi_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/nandi.png", width = 12, height = 8, dpi = 300)



# Baringo

baringo_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Baringo")

baringo_gcp_econ_activity_2023_top_5 <- baringo_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(baringo_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/baringo.png", width = 12, height = 8, dpi = 300)



# Laikipia

laikipia_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Laikipia")

laikipia_gcp_econ_activity_2023_top_5 <- laikipia_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(laikipia_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/laikipia.png", width = 12, height = 8, dpi = 300)



# Nakuru

nakuru_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Nakuru")

nakuru_gcp_econ_activity_2023_top_5 <- nakuru_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(nakuru_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/nakuru.png", width = 12, height = 8, dpi = 300)



# Narok

narok_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Narok")

narok_gcp_econ_activity_2023_top_5 <- narok_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(narok_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/narok.png", width = 12, height = 8, dpi = 300)



# Kajiado

kajiado_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kajiado")

kajiado_gcp_econ_activity_2023_top_5 <- kajiado_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kajiado_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kajiado.png", width = 12, height = 8, dpi = 300)


# Kericho

kericho_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kericho")

kericho_gcp_econ_activity_2023_top_5 <- kericho_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kericho_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kericho.png", width = 12, height = 8, dpi = 300)


# Bomet

bomet_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Bomet")

bomet_gcp_econ_activity_2023_top_5 <- bomet_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(bomet_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/bomet.png", width = 12, height = 8, dpi = 300)



# Kakamega

kakamega_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kakamega")

kakamega_gcp_econ_activity_2023_top_5 <- kakamega_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kakamega_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kakamega.png", width = 12, height = 8, dpi = 300)


# Vihiga

vihiga_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Vihiga")

vihiga_gcp_econ_activity_2023_top_5 <- vihiga_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(vihiga_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/vihiga.png", width = 12, height = 8, dpi = 300)


# Bungoma

bungoma_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Bungoma")

bungoma_gcp_econ_activity_2023_top_5 <- bungoma_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(bungoma_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/bungoma.png", width = 12, height = 8, dpi = 300)



# Busia

busia_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Busia")

busia_gcp_econ_activity_2023_top_5 <- busia_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(busia_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/busia.png", width = 12, height = 8, dpi = 300)



# Siaya

siaya_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Siaya")

siaya_gcp_econ_activity_2023_top_5 <- siaya_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(siaya_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/siaya.png", width = 12, height = 8, dpi = 300)



# Kisumu

kisumu_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kisumu")

kisumu_gcp_econ_activity_2023_top_5 <- kisumu_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kisumu_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kisumu.png", width = 12, height = 8, dpi = 300)



# Homa Bay

homa_bay_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Homa Bay")

homa_bay_gcp_econ_activity_2023_top_5 <- homa_bay_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(homa_bay_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/homa_bay.png", width = 12, height = 8, dpi = 300)



# Migori

migori_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Migori")

migori_gcp_econ_activity_2023_top_5 <- migori_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(migori_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/migori.png", width = 12, height = 8, dpi = 300)



# Kisii

kisii_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Kisii")

kisii_gcp_econ_activity_2023_top_5 <- kisii_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(kisii_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/kisii.png", width = 12, height = 8, dpi = 300)



# Nyamira

nyamira_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Nyamira")

nyamira_gcp_econ_activity_2023_top_5 <- nyamira_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(nyamira_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/nyamira.png", width = 12, height = 8, dpi = 300)


# Nairobi City

nairobi_city_gcp_econ_activity_2023 <- gcp_econ_activity_2023_select_tidy |>
  filter(county == "Nairobi City")

nairobi_city_gcp_econ_activity_2023_top_5 <- nairobi_city_gcp_econ_activity_2023 |>
  arrange(desc(contribution)) |>
  mutate(group = if_else(row_number() <= 5,
                         econ_activity, "Other Economic Activities")) |>
  group_by(group) |>
  summarise(contribution = sum(contribution)) |>
  mutate(percent_contribution = round((contribution/sum(contribution))*100, 1))

# Visualize the data

ggplot(nairobi_city_gcp_econ_activity_2023_top_5, 
       aes(area = percent_contribution, fill = percent_contribution, 
           label = paste0(group, "\n",
                          percent_contribution, "%"))) +
  geom_treemap(color = "black", size = 2) +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="azure2"),
        plot.background = element_rect(fill="azure2"),
        legend.background = element_rect(fill="azure2")) +
  scale_fill_gradient(low = "#EFD89F", high = "#DAA520")

ggsave("sub_pro_5_kenya_gcp_2024_analysis/images/gcp_econ_activity_2023_top_5/nairobi_city.png", width = 12, height = 8, dpi = 300)

