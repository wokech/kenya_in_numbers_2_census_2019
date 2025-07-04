# Kenyan Generations by Presidential Era (Rural) - PYRAMID
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

# Age-sex dataset
df_age <- V3_T2.2a
str(df_age)

# Age-sex dataset (individual years)
kenyan_pop_2019 <- df_age |>
  select(-Intersex) |>
  filter(Age != "Total" & Age != "Not Stated") |> 
  filter(!grepl("-", Age)) |>
  mutate(Age = if_else(Age == "100+", "100", Age))

kenyan_pop_2019$Age <- as.numeric(kenyan_pop_2019$Age)

#############################################################

# To create the groupings (if necessary)
# gen <- c('Kenyatta II', 'Kibaki', 'Moi', 'Kenyatta I', 'Pre-Independence')
# gen

# range <- c('> 2012', '2003-2012', '1979-2002', '1963-1978', '< 1963')
# range

# gen_desc <- data.frame(rank = 5:1,
#                        gen = gen,
#                        range = range,
#                        stringsAsFactors = FALSE) |>
# arrange(rank)

#############################################################

# Select the 3 different data types and add necessary columns

#############################################################
# i) Male
##############################################################

kenyan_pop_2019_male <- kenyan_pop_2019 |> select(Age, Male)
kenyan_pop_2019_male$type <- 'male'
kenyan_pop_2019_male$ref_year <- '2019'
k_pop_male <- kenyan_pop_2019_male |> 
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
  mutate (gen = case_when (
    birth_year < 1963 ~ 'Pre-Independence',
    birth_year < 1979 & birth_year >= 1963 ~ 'Kenyatta I',
    birth_year < 2003 & birth_year >= 1979 ~ 'Moi',
    birth_year <= 2012 & birth_year >= 2003 ~ 'Kibaki',
    birth_year > 2012 ~ 'Kenyatta II'),
    rank = case_when (
      birth_year < 1963 ~ '1',
      birth_year < 1979 & birth_year >= 1963 ~ '2',
      birth_year < 2003 & birth_year >= 1979 ~ '3',
      birth_year <= 2012 & birth_year >= 2003 ~ '4',
      birth_year > 2012 ~ '5'))

k_pop_male_gen$rank <- as.integer(k_pop_male_gen$rank)

###############################################################
# ii) Female
###############################################################

kenyan_pop_2019_female <- kenyan_pop_2019 |> select(Age, Female)
kenyan_pop_2019_female$type <- 'female'
kenyan_pop_2019_female$ref_year <- '2019'
k_pop_female <- kenyan_pop_2019_female |> 
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
  mutate (gen = case_when (
    birth_year < 1963 ~ 'Pre-Independence',
    birth_year < 1979 & birth_year >= 1963 ~ 'Kenyatta I',
    birth_year < 2003 & birth_year >= 1979 ~ 'Moi',
    birth_year <= 2012 & birth_year >= 2003 ~ 'Kibaki',
    birth_year > 2012 ~ 'Kenyatta II'),
    rank = case_when (
      birth_year < 1963 ~ '1',
      birth_year < 1979 & birth_year >= 1963 ~ '2',
      birth_year < 2003 & birth_year >= 1979 ~ '3',
      birth_year <= 2012 & birth_year >= 2003 ~ '4',
      birth_year > 2012 ~ '5'))

k_pop_female_gen$rank <- as.integer(k_pop_female_gen$rank)

#####################################################################
# iii) Total 
##################################################################### 

kenyan_pop_2019_total <- kenyan_pop_2019 |> select(Age, Total)
kenyan_pop_2019_total$type <- 'total'
kenyan_pop_2019_total$ref_year <- '2019' # reference year = 2019

k_pop_total <- kenyan_pop_2019_total |> 
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
  mutate (gen = case_when (
    birth_year < 1963 ~ 'Pre-Independence',
    birth_year < 1979 & birth_year >= 1963 ~ 'Kenyatta I',
    birth_year < 2003 & birth_year >= 1979 ~ 'Moi',
    birth_year <= 2012 & birth_year >= 2003 ~ 'Kibaki',
    birth_year > 2012 ~ 'Kenyatta II'),
    rank = case_when (
      birth_year < 1963 ~ '1',
      birth_year < 1979 & birth_year >= 1963 ~ '2',
      birth_year < 2003 & birth_year >= 1979 ~ '3',
      birth_year <= 2012 & birth_year >= 2003 ~ '4',
      birth_year > 2012 ~ '5'))

k_pop_total_gen$rank <- as.integer(k_pop_total_gen$rank)

############################################################
# C. Presidency Pyramid
############################################################

# Population by generation

# Male

p1 <- k_pop_male_gen |>
  group_by(gen, rank) |>
  summarize(population = sum(population)) |>
  mutate(lab = round(population/1000000, 2)) |>
  arrange(rank, gen) |>
  ggplot(aes(x = reorder(gen, -rank),
             y = -population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_void() + # Order matters put theme_void() before theme()
  labs(title = 'Male population (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 7,
            hjust = 1.1)+
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
                     breaks = c(0, -3000000, -6000000)) +
  scale_x_discrete(position = "top") 

p1

# Female

p2 <- k_pop_female_gen |>
  group_by(gen, rank) |>
  summarize(population = sum(population)) |>
  mutate(lab = round(population/1000000, 2)) |>
  arrange(rank, gen) |>
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_void() + # Order matters put theme_void() before theme()
  labs(title = 'Female population (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
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
                     breaks = c(0, 3000000, 6000000))

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

ggsave("sub_pro_2_pop_gen/images/pyramid_presidency_rural_1.png", width = 12, height = 12, dpi = 300)
