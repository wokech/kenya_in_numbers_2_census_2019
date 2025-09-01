# Kenya Teens (2019)
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
df_age <- V3_T2.2
str(df_age)

# Age-sex dataset (individual years)
kenyan_pop_2019 <- df_age |>
  select(-Intersex) |>
  filter(Age != "Total" & Age != "NotStated") |> 
  filter(!grepl("-", Age)) |>
  mutate(Age = if_else(Age == "100+", "100", Age))

kenyan_pop_2019$Age <- as.numeric(kenyan_pop_2019$Age)

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

k_pop_male_age_group <- k_pop_male |>
  mutate (age_group = case_when (
    birth_year < 2000 ~ 'Post-Teen',
    birth_year < 2007 & birth_year >= 2000 ~ 'Teen',
    birth_year >= 2007 ~ 'Pre-Teen'),
    rank = case_when (
      birth_year < 2000 ~ '1',
      birth_year < 2007 & birth_year >= 2000 ~ '2',
      birth_year >= 2007 ~ '3'))

k_pop_male_age_group$rank <- as.integer(k_pop_male_age_group$rank)

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

k_pop_female_age_group <- k_pop_female |>
  mutate (age_group = case_when (
    birth_year < 2000 ~ 'Post-Teen',
    birth_year < 2007 & birth_year >= 2000 ~ 'Teen',
    birth_year >= 2007 ~ 'Pre-Teen'),
    rank = case_when (
      birth_year < 2000 ~ '1',
      birth_year < 2007 & birth_year >= 2000 ~ '2',
      birth_year >= 2007 ~ '3'))

k_pop_female_age_group$rank <- as.integer(k_pop_female_age_group$rank)

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

k_pop_total_age_group <- k_pop_total |>
  mutate (age_group = case_when (
    birth_year < 2000 ~ 'Post-Teen',
    birth_year < 2007 & birth_year >= 2000 ~ 'Teen',
    birth_year >= 2007 ~ 'Pre-Teen'),
    rank = case_when (
      birth_year < 2000 ~ '1',
      birth_year < 2007 & birth_year >= 2000 ~ '2',
      birth_year >= 2007 ~ '3'))

k_pop_total_age_group$rank <- as.integer(k_pop_total_age_group$rank)

#####################################################################
# C. Plot the graphs
#####################################################################

# plot.margin in the theme function adjusts the margins around the entire plot.
# margin within axis.title.x and axis.title.y increases the space around axis titles.
# scale_y_continuous(expand = expansion(mult = c(0, 0.1))) adds extra space above the tallest bar, ensuring that the labels don't get cut off.

# i) Population by generation
# Male

p1 <- k_pop_male_age_group |>
  group_by(age_group, rank) |>
  summarize(population = sum(population)) |>
  mutate(lab = round(population/1000000, 2)) |>
  arrange(rank, age_group) |>
  ggplot(aes(x = reorder(age_group, -rank),
             y = population, 
             fill = age_group)) +
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
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.length.x = unit(3, "pt")) +
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_tableau()+
  scale_y_continuous(labels = comma, , expand = expansion(mult = c(0, 0.25)))

p1

# Female

p2 <- k_pop_female_age_group |>
  group_by(age_group, rank) |>
  summarize(population = sum(population)) |>
  mutate(lab = round(population/1000000, 2)) |>
  arrange(rank, age_group) |>
  ggplot(aes(x = reorder(age_group, -rank),
             y = population, 
             fill = age_group)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_void() + # Order matters put theme_void() before theme()
  labs(title = 'Female population grouped by generation (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 6,
            hjust = -0.1)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.length.x = unit(3, "pt")) +
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_tableau()+
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25)))

p2

# Total

p3 <- k_pop_total_age_group |>
  group_by(age_group, rank) |>
  summarize(population = sum(population)) |>
  mutate(lab = round(population/1000000, 2)) |>
  arrange(rank, age_group) |>
  ggplot(aes(x = reorder(age_group, -rank),
             y = population, 
             fill = age_group)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_void() + # Order matters put theme_void() before theme()
  labs(title = 'Population grouped by generation (2019)', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 6, 
            hjust = -0.1)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.length.x = unit(3, "pt")) +
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_tableau()+
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25)))

p3

#######################################################################
# ii) Population by single year of age & generation
########################################################################

# Male
gg1 <- k_pop_male_age_group |> 
  group_by(birth_year, age, age_group) |>
  summarize(tot = sum(population)) |>
  group_by(age_group) |>
  mutate(tot = max(tot)) |> #For labels below.
  filter(birth_year %in% c('1919', '1999', '2006', '2019'))
#View(gg1)

# Female
gg2 <- k_pop_female_age_group |> 
  group_by(birth_year, age, age_group) |>
  summarize(tot = sum(population)) |>
  group_by(age_group) |>
  mutate(tot = max(tot)) |> #For labels below.
  filter(birth_year %in% c('1919', '1999', '2006', '2019'))
#View(gg2)

# Total
gg3 <- k_pop_total_age_group |> 
  group_by(birth_year, age, age_group) |>
  summarize(tot = sum(population)) |>
  group_by(age_group) |>
  mutate(tot = max(tot)) |> #For labels below.
  filter(birth_year %in% c('1919', '1999', '2006', '2019'))
#View(gg3)

# Male

p4 <- k_pop_male_age_group |>
  ggplot(aes(x = age, 
             y = population, 
             fill = age_group)) +
  geom_vline(xintercept = gg1$age,
             linetype =2, 
             color = 'black', 
             linewidth = .5) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
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
        axis.title.y = element_text(size=20, face = "bold", angle = 90, margin = margin(r = 0)),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(5, 5, 5, 5),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  ggthemes::scale_fill_tableau()+
  scale_x_reverse(breaks = rev(gg1$age)) +
  scale_y_continuous(labels = comma) +
  labs(title = 'Male population grouped by single-year age & generation (2019)')

p4

# Female

p5 <- k_pop_female_age_group |>
  ggplot(aes(x = age, 
             y = population, 
             fill = age_group)) +
  geom_vline(xintercept = gg2$age,
             linetype =2, 
             color = 'black', 
             linewidth = .5) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  xlab('Age')+ 
  ylab('Population') +
  theme_void() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold", angle = 90),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(5, 5, 5, 5),
        plot.background = element_rect(fill = "azure2", color = "azure2")) +
  ggthemes::scale_fill_tableau() +
  scale_x_reverse(breaks = rev(gg2$age)) +
  scale_y_continuous(labels = comma) +
  labs(title = 'Female population grouped by single-year age & generation (2019)')

p5

# Total

p6 <- k_pop_total_age_group |>
  ggplot(aes(x = age, 
             y = population, 
             fill = age_group)) +
  geom_vline(xintercept = gg3$age,
             linetype =2, 
             color = 'black', 
             linewidth = .5) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  xlab('Age')+ 
  ylab('Population') +
  theme_void() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
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

# Male
p4 / p1 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_blank(),
                                plot.subtitle = element_blank(),
                                plot.caption = element_blank(),
                                plot.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_2_pop_gen/images/teens/knbs_teens_male_1.png", width = 12, height = 12, dpi = 300)

# Female
p5 / p2 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_blank(),
                                plot.subtitle = element_blank(),
                                plot.caption = element_blank(),
                                plot.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_2_pop_gen/images/teens/knbs_teens_female_1.png", width = 12, height = 12, dpi = 300)

# Total
p6 / p3 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_blank(),
                                plot.subtitle = element_blank(),
                                plot.caption = element_blank(),
                                plot.background = element_rect(fill = "azure2", color = "azure2"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_2_pop_gen/images/teens/knbs_teens_total_1.png", width = 12, height = 12, dpi = 300)
