# What type of homes do Kenyans live in? Tenure, Mode of Acquisition, and House Type.
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_1 <- V4_T2.10
View(df_1)

# 4) Preliminary filtering and cleanup

# Table 1 for National Analysis
table_1 <- df_1[1:3,]
View(table_1)

table_1 <- table_1 %>%
  clean_names()

table_1_select <- table_1 %>%
  select(county, sub_county, ch_owned_perc, ch_rent_provided_perc) 
  
View(table_1_select)

# Pie chart

table_1_select_tidy <- table_1_select %>%
  pivot_longer(c(ch_owned_perc, ch_rent_provided_perc), 
               names_to = "tenure", values_to = "percentage") %>%
  mutate(tenure = ifelse(tenure == "ch_owned_perc", "Own",
                        ifelse(tenure == "ch_rent_provided_perc", "Rent", tenure)))



table_1_pie <- data.frame(
  group = table_1_select_tidy$tenure[1:2],
  percentage = table_1_select_tidy$percentage[1:2]
)

table_1_pie

# Compute the position of labels
table_1_pie <- table_1_pie %>% 
  arrange(desc(group)) %>%
  mutate(prop = percentage / sum(table_1_pie$percentage) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(table_1_pie, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
    geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

ggsave("images/homes_national/national_pie.png", width = 5, height = 5)


# Stacked bar chart 

table_1_select_no <- table_1 %>%
  select(county, sub_county, ch_owned_no, ch_rent_provided_no) 

View(table_1_select_no)

table_1_select_no_tidy <- table_1_select_no %>%
  pivot_longer(c(ch_owned_no, ch_rent_provided_no), 
               names_to = "tenure", values_to = "number") %>%
  mutate(tenure = ifelse(tenure == "ch_owned_no", "Own",
                  ifelse(tenure == "ch_rent_provided_no", "Rent", tenure)))
table_1_select_no_tidy

table_1_no_stacked <- table_1_select_no_tidy[3:6,] 

table_1_no_stacked

ggplot(table_1_no_stacked, aes(fill=sub_county, y=number, x=tenure)) + 
  geom_bar(position="fill", stat="identity") + 
  coord_flip() + 
theme_minimal() +
  labs(x = "", 
       y = "Percentage (%)", 
       title = "Where do Kenyans own and where do they rent?",
       subtitle = "Percentage of Kenyans who own or rent their homes in both rural and urban regions",
       caption = "Data Source: rkenyaCensus | By: @willyokech",
       fill = "") +
  theme(axis.title.x =element_text(size = 18),
        axis.title.y =element_text(size = 18),
        axis.text.x =element_text(size = 14),
        axis.text.y =element_text(size = 24),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 24),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 18),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette="Reds")

ggsave("images/homes_national/rur_urb_stacked.png", width = 5, height = 5)

  