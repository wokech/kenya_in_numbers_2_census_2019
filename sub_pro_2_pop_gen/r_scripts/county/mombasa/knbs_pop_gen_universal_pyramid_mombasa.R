# Kenyan Generations by Universal Classification (Mombasa) - PYRAMID
# Author: William Okech

################################################
# A. Load libraries
###############################################

# install.packages("readr")
# install.packages("patchwork")
# install.packages("ggthemes")
library(readr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggthemes)
library(scales)
# install.packages("devtools")
# devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)

#############################################################
# B. Load the required data from 2019 Census
############################################################

#############################################################
# B. Load the required data from 2019 Census
############################################################

# Age-sex dataset
df_age_sex <- V3_T2.3
str(df_age_sex)

# Age-sex dataset cleanup

county_df_age_sex <- df_age_sex |>
  filter(SubCounty == "ALL") |>
  select(County, Age, Male, Female, Total) |>
  filter(Age != "Total" & Age != "Not Stated") |> 
  filter(!grepl("-", Age)) |>
  mutate(Age = if_else(Age == "100+", "100", Age)) |>
  mutate(County = gsub("/ ", " ", County)) |>
  mutate(County = gsub("-", " ", County)) |>         
  mutate(County = tools::toTitleCase(tolower(County))) |>
  mutate(County = trimws(County))

# Age-sex dataset for Mombasa

county_df_age_sex_mombasa <- county_df_age_sex |>
  filter(County == "Mombasa")

county_df_age_sex_mombasa$Age <- as.numeric(county_df_age_sex_mombasa$Age)

#############################################################

# # To create the groupings (if necessary)
# gen <- c('Post-Z', 'Gen Z', 'Millennial',
#          'Gen X', 'Boomers', 'Silent',
#          'Greatest')
# 
# range <- c('> 2012', '1997-2012', '1981-1996',
#            '1965-1980', '1946-1964', '1928-1945',
#            '< 1927')
# 
# gen_desc <- data.frame(rank = 7:1,
#                        gen = gen,
#                        range = range,
#                        stringsAsFactors = FALSE) |>
#   arrange(rank)

#############################################################

# Select the 3 different data types and add necessary columns

#############################################################
# i) Male
##############################################################

county_df_age_sex_mombasa_male <- county_df_age_sex_mombasa |> select(Age, Male)
county_df_age_sex_mombasa_male$type <- 'male'
county_df_age_sex_mombasa_male$ref_year <- '2019'
k_pop_male <- county_df_age_sex_mombasa_male |> 
  rename(
    age = Age,
    population = Male,
    type = type
  )

k_pop_male$age <- as.integer(k_pop_male$age)
k_pop_male$population <- as.integer(k_pop_male$population)
k_pop_male$ref_year <- as.integer(k_pop_male$ref_year)
k_pop_male$birth_year <- k_pop_male$ref_year - k_pop_male$age

k_pop_male_gen <- k_pop_male |>
  mutate(gen = case_when (
    birth_year < 1928 ~ 'Greatest',
    birth_year < 1946 & birth_year >= 1928 ~ 'Silent',
    birth_year < 1965 & birth_year >= 1946 ~ 'Boomer',
    birth_year < 1981 & birth_year >= 1965 ~ 'Gen X',
    birth_year < 1996 & birth_year >= 1981 ~ 'Millenial',
    birth_year < 2013 & birth_year >= 1996 ~ 'Gen Z',
    birth_year > 2012 ~ 'Gen Alpha'),
    rank = case_when (
      birth_year < 1928 ~ '1',
      birth_year < 1946 & birth_year >= 1928 ~ '2',
      birth_year < 1965 & birth_year >= 1946 ~ '3',
      birth_year < 1981 & birth_year >= 1965 ~ '4',
      birth_year < 1996 & birth_year >= 1981 ~ '5',
      birth_year < 2013 & birth_year >= 1996 ~ '6',
      birth_year > 2012 ~ '7'))


k_pop_male_gen$rank <- as.integer(k_pop_male_gen$rank)

###############################################################
# ii) Female
###############################################################

county_df_age_sex_mombasa_female <- county_df_age_sex_mombasa |> select(Age, Female)
county_df_age_sex_mombasa_female$type <- 'female'
county_df_age_sex_mombasa_female$ref_year <- '2019'
k_pop_female <- county_df_age_sex_mombasa_female |> 
  rename(
    age = Age,
    population = Female,
    type = type
  )

k_pop_female$age <- as.integer(k_pop_female$age)
k_pop_female$population <- as.integer(k_pop_female$population)
k_pop_female$ref_year <- as.integer(k_pop_female$ref_year)
k_pop_female$birth_year <- k_pop_female$ref_year - k_pop_female$age

k_pop_female_gen <- k_pop_female |>
  mutate(gen = case_when (
    birth_year < 1928 ~ 'Greatest',
    birth_year < 1946 & birth_year >= 1928 ~ 'Silent',
    birth_year < 1965 & birth_year >= 1946 ~ 'Boomer',
    birth_year < 1981 & birth_year >= 1965 ~ 'Gen X',
    birth_year < 1996 & birth_year >= 1981 ~ 'Millenial',
    birth_year < 2013 & birth_year >= 1996 ~ 'Gen Z',
    birth_year > 2012 ~ 'Gen Alpha'),
    rank = case_when (
      birth_year < 1928 ~ '1',
      birth_year < 1946 & birth_year >= 1928 ~ '2',
      birth_year < 1965 & birth_year >= 1946 ~ '3',
      birth_year < 1981 & birth_year >= 1965 ~ '4',
      birth_year < 1996 & birth_year >= 1981 ~ '5',
      birth_year < 2013 & birth_year >= 1996 ~ '6',
      birth_year > 2012 ~ '7'))

k_pop_female_gen$rank <- as.integer(k_pop_female_gen$rank)

#####################################################################
# iii) Total 
#####################################################################

county_df_age_sex_mombasa_total <- county_df_age_sex_mombasa |> select(Age, Total)
county_df_age_sex_mombasa_total$type <- 'total'
county_df_age_sex_mombasa_total$ref_year <- '2019' # reference year = 2019

k_pop_total <- county_df_age_sex_mombasa_total |> 
  rename(
    age = Age,
    population = Total,
    type = type
  )

k_pop_total$age <- as.integer(k_pop_total$age)
k_pop_total$population <- as.integer(k_pop_total$population)
k_pop_total$ref_year <- as.integer(k_pop_total$ref_year)
k_pop_total$birth_year <- k_pop_total$ref_year - k_pop_total$age

k_pop_total_gen <- k_pop_total |>
  mutate(gen = case_when (
    birth_year < 1928 ~ 'Greatest',
    birth_year < 1946 & birth_year >= 1928 ~ 'Silent',
    birth_year < 1965 & birth_year >= 1946 ~ 'Boomer',
    birth_year < 1981 & birth_year >= 1965 ~ 'Gen X',
    birth_year < 1996 & birth_year >= 1981 ~ 'Millenial',
    birth_year < 2013 & birth_year >= 1996 ~ 'Gen Z',
    birth_year >= 2013 ~ 'Gen Alpha'),
    rank = case_when (
      birth_year < 1928 ~ '1',
      birth_year < 1946 & birth_year >= 1928 ~ '2',
      birth_year < 1965 & birth_year >= 1946 ~ '3',
      birth_year < 1981 & birth_year >= 1965 ~ '4',
      birth_year < 1996 & birth_year >= 1981 ~ '5',
      birth_year < 2013 & birth_year >= 1996 ~ '6',
      birth_year >= 2013 ~ '7'))

k_pop_total_gen$rank <- as.integer(k_pop_total_gen$rank)

############################################################
# C. Universal Classification Pyramid
############################################################

# Population by generation

# Male

p1 <- k_pop_male_gen |>
  group_by(gen, rank) |>
  summarize(population = sum(population)) |>
  mutate(lab = round(population/1000, 2)) |>
  arrange(rank, gen) |>
  ggplot(aes(x = reorder(gen, -rank),
             y = -population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_void() + # Order matters put theme_void() before theme()
  labs(title = 'Male population (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "K")), 
            size = 7, 
            hjust = 1.2)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20, margin = margin(l = 15)),
        axis.title.y = element_text(margin = margin(r = 20)),  # Increase right margin
        plot.title = element_text(face = "bold", size = 20, hjust = 1),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.length.x = unit(3, "pt"),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_tableau()+
  scale_y_continuous(labels = function(x) comma(abs(x)), 
                     expand = expansion(mult = c(0.25, 0)),
                     breaks = c(0, -150000, -300000)) +
  scale_x_discrete(position = "top") 

p1

# Female

p2 <- k_pop_female_gen |>
  group_by(gen, rank) |>
  summarize(population = sum(population)) |>
  mutate(lab = round(population/1000, 2)) |>
  arrange(rank, gen) |>
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_void() + # Order matters put theme_minimal() before theme()
  labs(title = 'Female population (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "K")), 
            size = 7, 
            hjust = -0.1)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.length.x = unit(3, "pt"),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_tableau()+
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.25)),
                     breaks = c(0, 150000, 300000))

p2 

# Combi Plot

p1 + p2 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_blank(),
                                plot.subtitle = element_blank(),
                                plot.caption = element_blank(),
                                plot.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_2_pop_gen/images/county/mombasa/pyramid_universal_mombasa_1.png", width = 12, height = 12, dpi = 300)

