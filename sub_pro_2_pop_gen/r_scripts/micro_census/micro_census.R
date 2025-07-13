# Micro-Census Work

library(tidyverse)
library(openxlsx) # Use it to preserve the whitespaces
library(janitor)

# Load the required data

pop_micro_census <- read.xlsx("sub_pro_2_pop_gen/datasets/micro_census.xlsx")

# Leading Spaces 

pop_micro_census <- pop_micro_census |>
  mutate(leading_spaces = str_extract(Name, "^\\s*") |> str_length()) |>
  mutate(hierachy_levels = case_when(
    leading_spaces == 0 ~ "Country",
    leading_spaces == 4 ~ "County",
    leading_spaces == 8 ~ "Subcounty",
    leading_spaces == 12 ~ "Division",
    leading_spaces == 16 ~ "Location",
    leading_spaces == 20 ~ "Sublocation",
    TRUE ~ NA_character_)) |>
  mutate(Name = tools::toTitleCase(tolower(Name)))

# Rearrange the columns

pop_micro_census <- pop_micro_census |>
  rename("Leading Spaces" = leading_spaces,
         "Hierachy Levels" = hierachy_levels) |>
  select(Name, "Leading Spaces", "Hierachy Levels", everything())

#### Better definition of the name column ####

# Helper function to count indentation
count_leading_spaces <- function(x) {
  nchar(x) - nchar(str_trim(x, side = "left"))
}

# Define hierarchy levels
level_map <- c("Country", "County", "Subcounty", "Division", "Location", "Sublocation")

# Process initial levels
pop_micro_census_processed <- pop_micro_census |>
  mutate(
    indent = count_leading_spaces(Name),
    level = indent / 4 + 1,
    level_name = level_map[level],
    clean_name = str_trim(Name)
  )

# Initialize all levels as columns
for (lvl in level_map) {
  pop_micro_census_processed[[lvl]] <- NA_character_
}

# Track current hierarchy
current <- rep(NA_character_, length(level_map))
for (i in seq_len(nrow(pop_micro_census_processed))) {
  lvl <- pop_micro_census_processed$level[i]
  current[lvl] <- pop_micro_census_processed$clean_name[i]
  if (lvl < length(level_map)) {
    current[(lvl + 1):length(level_map)] <- NA_character_
  }
  pop_micro_census_processed[i, level_map] <- current
}

# Exclude 'country' and build reversed full path
pop_micro_census_processed <- pop_micro_census_processed |>
  filter(level_name != "Country") 

# Create comma-separated, reversed path (excluding country)

hierarchy_cols <- rev(level_map[-1])

# Reverse and remove the country

pop_micro_census_processed <- pop_micro_census_processed |>
  mutate(
    full_name = apply(pop_micro_census_processed[, hierarchy_cols]
                      , 1, function(x) paste(na.omit(x), 
                                             collapse = ", "))
  ) 
  
# Clean column names

pop_micro_census_processed <- pop_micro_census_processed |>
  clean_names()
  
pop_micro_census_processed <- pop_micro_census_processed |> 
  select(name, hierachy_levels, full_name, c(total:population_density_per_sq_km), everything()) |>
  mutate(across(c(group_quarters_households, 
                  population_density_per_sq_km),
                as.numeric))

write.xlsx(pop_micro_census_processed,
           file = "sub_pro_2_pop_gen/datasets/micro_census_processed.xlsx",
           keepNA = TRUE)



