# Kenyan Generations by Universal Classification (Rural)
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

#####################################################################
# C. Plot the graphs
#####################################################################

# plot.margin in the theme function adjusts the margins around the entire plot.
# margin within axis.title.x and axis.title.y increases the space around axis titles.
# scale_y_continuous(expand = expansion(mult = c(0, 0.1))) adds extra space above the tallest bar, ensuring that the labels don't get cut off.

# i) Population by generation
# Male

p1 <- k_pop_male_gen |>
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
  labs(title = 'Male population grouped by generation (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 6, 
            hjust = -0.1)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.length.x = unit(3, "pt"),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_tableau()+
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25)))

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
  theme_void() + # Order matters put theme_minimal() before theme()
  labs(title = 'Female population grouped by generation (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 6, 
            hjust = -0.1)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.length.x = unit(3, "pt"),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_tableau()+
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25)))

p2

# Total

p3 <- k_pop_total_gen |>
  group_by(gen, rank) |>
  summarize(population = sum(population)) |>
  mutate(lab = round(population/1000000, 2)) |>
  arrange(rank, gen) |>
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_void() +
  labs(title = 'Population grouped by generation (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 6,
            hjust = -0.1) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.length.x = unit(3, "pt")) +
  coord_flip() +
  ggthemes::scale_fill_tableau() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25)))

p3

#######################################################################
# ii) Population by single year of age & generation
########################################################################

# Male
gg1 <- k_pop_male_gen |> 
  group_by(birth_year, age, gen) |>
  summarize(tot = sum(population)) |>
  group_by(gen) |>
  mutate(tot = max(tot)) |> #For labels below.
  filter(birth_year %in% c('1919', '1928', '1946', '1965', '1981', '1996', '2013'))
#View(gg1)

# Female
gg2 <- k_pop_female_gen |> 
  group_by(birth_year, age, gen) |>
  summarize(tot = sum(population)) |>
  group_by(gen) |>
  mutate(tot = max(tot)) |> #For labels below.
  filter(birth_year %in% c('1919', '1928', '1946', '1965', '1981', '1996', '2013'))
#View(gg2)

# Total
gg3 <- k_pop_total_gen |> 
  group_by(birth_year, age, gen) |>
  summarize(tot = sum(population)) |>
  group_by(gen) |>
  mutate(tot = max(tot)) |> #For labels below.
  filter(birth_year %in% c('1919', '1928', '1946', '1965', '1981', '1996', '2013'))
#View(gg3)

# Male

p4 <- k_pop_male_gen |>
  ggplot(aes(x = age, 
             y = population, 
             fill = gen)) +
  geom_vline(xintercept = gg1$age,
             linetype =2, 
             color = 'black', 
             linewidth = .5) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  annotate(geom="text", 
           x = gg1$age - 5, 
           y = gg1$tot + 40000, 
           label = gg1$gen,
           size = 5) +
  xlab('Age')+ 
  ylab('Population') +
  theme_void() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold", angle = 90),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(5, 5, 5, 5),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  ggthemes::scale_fill_tableau()+
  scale_x_reverse(breaks = rev(gg1$age)) +
  scale_y_continuous(labels = comma) +
  labs(title = 'Male population grouped by single-year age & generation (2019)')

p4

# Female

p5 <- k_pop_female_gen |>
  ggplot(aes(x = age, 
             y = population, 
             fill = gen)) +
  geom_vline(xintercept = gg2$age,
             linetype =2, 
             color = 'black', 
             linewidth = .5) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  annotate(geom="text", 
           x = gg2$age - 5, 
           y = gg2$tot + 40000, 
           label = gg2$gen,
           size = 5) +
  xlab('Age')+ 
  ylab('Population') +
  theme_void() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold", angle = 90),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(5, 5, 5, 5),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  ggthemes::scale_fill_tableau() +
  scale_x_reverse(breaks = rev(gg2$age)) +
  scale_y_continuous(labels = comma) +
  labs(title = 'Female population grouped by single-year age & generation (2019)')

p5

# Total

p6 <- k_pop_total_gen |>
  ggplot(aes(x = age, 
             y = population, 
             fill = gen)) +
  geom_vline(xintercept = gg3$age,
             linetype =2, 
             color = 'black', 
             linewidth = .5) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  annotate(geom="text", 
           x = gg3$age - 5, 
           y = gg3$tot + 40000, 
           label = gg3$gen,
           size = 5) +
  xlab('Age') + 
  ylab('Population') +
  theme_void() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold", angle = 90),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(5, 5, 5, 5),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  ggthemes::scale_fill_tableau()+
  scale_x_reverse(breaks = rev(gg3$age)) +
  scale_y_continuous(labels = comma) +
  labs(title = 'Population grouped by single-year age & generation (2019)')

p6

#########################################################################
# D. Production Images
########################################################################


# i) Male
p4 / p1 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_blank(),
                                plot.subtitle = element_blank(),
                                plot.caption = element_blank(),
                                plot.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_2_pop_gen/images/rural/knbs_pop_generation_2019_universal_rural_1.png", width = 12, height = 12, dpi = 300)

# ii) Female
p5 / p2 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_blank(),
                                plot.subtitle = element_blank(),
                                plot.caption = element_blank(),
                                plot.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_2_pop_gen/images/rural/knbs_pop_generation_2019_universal_rural_2.png", width = 12, height = 12, dpi = 300)

# iii) Total

p6 / p3 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_blank(),
                                plot.subtitle = element_blank(),
                                plot.caption = element_blank(),
                                plot.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_2_pop_gen/images/rural/knbs_pop_generation_2019_universal_rural_3.png", width = 12, height = 12, dpi = 300)
