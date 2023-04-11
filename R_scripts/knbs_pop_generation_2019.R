# Kenyan Generations by Presidential Era 
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

#############################################################
# B. Load the required data from 2019 Census
############################################################

# Age-sex dataset
df_age <- V3_T2.2
str(df_age)

# Age-sex dataset (individual years)
kenyan_pop_2019 <- df_age %>%
  select(-Intersex) %>%
  filter(Age != "Total" & Age != "NotStated") %>% 
  filter(!grepl("-", Age)) %>%
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
#                        stringsAsFactors = FALSE) %>%
# arrange(rank)

#############################################################

# Select the 3 different data types and add necessary columns

#############################################################
# i) Male
##############################################################

kenyan_pop_2019_male <- kenyan_pop_2019 %>% select(Age, Male)
kenyan_pop_2019_male$type <- 'male'
kenyan_pop_2019_male$ref_year <- '2019'
k_pop_male <- kenyan_pop_2019_male %>% 
  rename(
    age = Age,
    population = Male,
    type = type
  )

k_pop_male$age <- as.integer(k_pop_male$age)
k_pop_male$population <- as.integer(k_pop_male$population)
k_pop_male$ref_year <- as.integer(k_pop_male$ref_year)
k_pop_male$birth_year <- k_pop_male$ref_year - k_pop_male$age

k_pop_male_gen <- k_pop_male %>%
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

kenyan_pop_2019_female <- kenyan_pop_2019 %>% select(Age, Female)
kenyan_pop_2019_female$type <- 'female'
kenyan_pop_2019_female$ref_year <- '2019'
k_pop_female <- kenyan_pop_2019_female %>% 
  rename(
    age = Age,
    population = Female,
    type = type
  )

k_pop_female$age <- as.integer(k_pop_female$age)
k_pop_female$population <- as.integer(k_pop_female$population)
k_pop_female$ref_year <- as.integer(k_pop_female$ref_year)
k_pop_female$birth_year <- k_pop_female$ref_year - k_pop_female$age

k_pop_female_gen <- k_pop_female %>%
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

kenyan_pop_2019_total <- kenyan_pop_2019 %>% select(Age, Total)
kenyan_pop_2019_total$type <- 'total'
kenyan_pop_2019_total$ref_year <- '2019' # reference year = 2019

k_pop_total <- kenyan_pop_2019_total %>% 
  rename(
    age = Age,
    population = Total,
    type = type
  )

k_pop_total$age <- as.integer(k_pop_total$age)
k_pop_total$population <- as.integer(k_pop_total$population)
k_pop_total$ref_year <- as.integer(k_pop_total$ref_year)
k_pop_total$birth_year <- k_pop_total$ref_year - k_pop_total$age

k_pop_total_gen <- k_pop_total %>%
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

#####################################################################
# C. Plot the graphs
#####################################################################

# i) Population by generation
# Male

p1 <- k_pop_male_gen %>%
  group_by(gen, rank) %>%
  summarize(population = sum(population)) %>%
  mutate(lab = round(population/1000000, 2)) %>%
  arrange(rank, gen) %>%
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_minimal() + # Order matters put theme_minimal() before theme()
  labs(title = 'Male population grouped by generation', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 5)+
  theme(#axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))+
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_stata()+
  scale_y_continuous(labels = comma)

p1

# Female

p2 <- k_pop_female_gen %>%
  group_by(gen, rank) %>%
  summarize(population = sum(population)) %>%
  mutate(lab = round(population/1000000, 2)) %>%
  arrange(rank, gen) %>%
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_minimal() + # Order matters put theme_minimal() before theme()
  labs(title = 'Female population grouped by generation', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 5)+
  theme(#axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))+
  xlab('') + 
  ylab('') +
  coord_flip()+
  ggthemes::scale_fill_stata() +
  scale_y_continuous(labels = comma)

p2

# Total

p3 <- k_pop_total_gen %>%
  group_by(gen, rank) %>%
  summarize(population = sum(population)) %>%
  mutate(lab = round(population/1000000, 2)) %>%
  arrange(rank, gen) %>%
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  theme_minimal() +
  labs(title = 'Population grouped by generation', caption = '') +
  geom_text(aes(label = paste(lab, "M")), 
            size = 5, hjust = 0.25)+
  theme(#axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))+
  xlab('') + ylab('') +
  coord_flip()+
  ggthemes::scale_fill_stata() +
  scale_y_continuous(labels = comma)

p3

#######################################################################
# ii) Population by single year of age & generation
########################################################################

# Male
gg1 <- k_pop_male_gen %>% 
  group_by(birth_year, age, gen) %>%
  summarize(tot = sum(population)) %>%
  group_by(gen) %>%
  mutate(tot = max(tot)) %>% #For labels below.
  filter(birth_year %in% c('1919', '1963', '1979', '2003', '2013'))
#View(gg1)

# Female
gg2 <- k_pop_female_gen %>% 
  group_by(birth_year, age, gen) %>%
  summarize(tot = sum(population)) %>%
  group_by(gen) %>%
  mutate(tot = max(tot)) %>% #For labels below.
  filter(birth_year %in% c('1919', '1963', '1979', '2003', '2013'))
#View(gg2)

# Total
gg3 <- k_pop_total_gen %>% 
  group_by(birth_year, age, gen) %>%
  summarize(tot = sum(population)) %>%
  group_by(gen) %>%
  mutate(tot = max(tot)) %>% #For labels below.
  filter(birth_year %in% c('1919', '1963', '1979', '2003', '2013'))
#View(gg3)

# Male

p4 <- k_pop_male_gen %>%
  ggplot(aes(x = age, 
             y = population, 
             fill = gen)) +
  geom_vline(xintercept = gg1$age,
             linetype =2, 
             color = 'gray', 
             size = .25) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  annotate(geom="text", 
           x = gg1$age - 4.5, 
           y = gg1$tot + 70000, 
           label = gg1$gen,
           size = 5) +
  xlab('Age')+ 
  ylab('Population') +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=16, face = "bold"),
        axis.title.y = element_text(size=16, face = "bold")) +
  ggthemes::scale_fill_stata()+
  scale_x_reverse(breaks = rev(gg1$age)) +
  scale_y_continuous(labels = comma) +
  labs(title = 'Male population grouped by single-year age & generation')

p4

# Female

p5 <- k_pop_female_gen %>%
  ggplot(aes(x = age, 
             y = population, 
             fill = gen)) +
  geom_vline(xintercept = gg2$age,
             linetype =2, 
             color = 'gray', 
             size = .25) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  annotate(geom="text", 
           x = gg2$age - 4.5, 
           y = gg2$tot + 70000, 
           label = gg2$gen,
           size = 5) +
  xlab('Age')+ 
  ylab('Population') +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold")) +
  ggthemes::scale_fill_stata() +
  scale_x_reverse(breaks = rev(gg2$age)) +
  scale_y_continuous(labels = comma) +
  labs(title = 'Female population grouped by single-year age & generation')

p5

# Total

p6 <- k_pop_total_gen %>%
  ggplot(aes(x = age, 
             y = population, 
             fill = gen)) +
  geom_vline(xintercept = gg3$age,
             linetype =2, 
             color = 'gray', 
             size = .25) +
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  annotate(geom="text", 
           x = gg3$age - 4.5, 
           y = gg3$tot + 70000, 
           label = gg3$gen,
           size = 5) +
  xlab('Age')+ 
  ylab('Population') +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size=16, face = "bold"),
        axis.title.y = element_text(size=16, face = "bold")) +
  ggthemes::scale_fill_stata()+
  scale_x_reverse(breaks = rev(gg3$age)) +
  scale_y_continuous(labels = comma) +
  labs(title = 'Population grouped by single-year age & generation')

p6

#########################################################################
# D. Production Images
########################################################################

# Male
p4 / p1 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "Source: rKenyaCensus | By: @willyokech\nInspired by Jason Timm (https://jtimm.net/posts/seven-generations/)",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "beige"))) &
  theme(text = element_text('Helvetica'))
  

ggsave("images/knbs_pop_generation_2019/knbs_pop_generation_2019_1.png", width = 10, height = 7.5)

# Female
p5 / p2 +
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "Source: rKenyaCensus | By: @willyokech\nInspired by Jason Timm (https://jtimm.net/posts/seven-generations/)",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "beige"))) &
  theme(text = element_text('Helvetica'))

ggsave("images/knbs_pop_generation_2019/knbs_pop_generation_2019_2.png", width = 10, height = 7.5)

# Total
p6 / p3 +
  plot_annotation(title = "Majority of Kenyans living today were born during the Moi era",
                  subtitle = "",
                  caption = "Source: rKenyaCensus | By: @willyokech\nInspired by Jason Timm (https://jtimm.net/posts/seven-generations/)",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12),
                                plot.background = element_rect(fill = "beige"))) &
  theme(text = element_text('Helvetica'))

ggsave("images/knbs_pop_generation_2019/knbs_pop_generation_2019_3.png", width = 10, height = 7.5)

