# Continuation of county_sex_census.R
library(patchwork)
library(tidyverse)
library(ggbreak)

# 1) Load the subcounty sex census data and clean

df_2 <- V1_T2.5
df_2


# Calculate the male:female ratio per 100
df_2_ratio <- df_2 %>%
  mutate(m_f_ratio = Male/Female,
         m_f_ratio_100 = round(m_f_ratio*100, 0))

# Remove the "Total" row and include the "Subcounty"
df_2_ratio_subcounty <- df_2_ratio %>%
  filter(AdminArea == "SubCounty") %>%
  select(County, SubCounty, m_f_ratio_100, Total) %>%
  filter(County != "Total")

# Find the top 10 subcounties
top_subcounty <- df_2_ratio_subcounty %>%
  unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
  arrange(desc(m_f_ratio_100)) %>%
  slice(1:20)
View(top_subcounty)

top_subcounty_plot <- top_subcounty %>%
  ggplot(aes(x = reorder(county_sub, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "lightblue") + 
  coord_flip() + 
  scale_y_break(c(40, 80)) + 
  theme_classic()+
  labs(x = "Subcounty", 
       y = "Number of males per 100 females", 
       title = "Top 20 Subcounties",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

top_subcounty_plot

ggsave("images/subcounty_sex_census/top_subcounty_plot.png", width = 6, height = 4)

# Find the bottom 10 subcounties
bottom_subcounty <- df_2_ratio_subcounty %>%
  unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
  arrange(m_f_ratio_100) %>%
  slice(1:20)
View(bottom_subcounty)


bottom_subcounty_plot <- bottom_subcounty %>%
  ggplot(aes(x = reorder(county_sub, m_f_ratio_100), y = m_f_ratio_100)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "blue") + 
  coord_flip() + 
  scale_y_break(c(40, 80)) + 
  theme_classic()+
  labs(x = "Subcounty", 
       y = "Number of males per 100 females", 
       title = "Bottom 20 Subcounties",
       caption = "") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) + 
  geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5)

bottom_subcounty_plot

ggsave("images/subcounty_sex_census/bottom_subcounty_plot.png", width = 6, height = 4)

top_subcounty_plot + bottom_subcounty_plot

ggsave("images/subcounty_sex_census/top_bottom_plot.png", width = 12, height = 4)

