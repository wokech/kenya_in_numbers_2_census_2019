# Old notes on mapping by Shel Kariuki

# Load the downloaded shapefiles 
KenyaSHP <- read_sf("kenyan-counties/County.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)

# To easily view the shapefile in RStudio View pane, you can drop the geometry column and view the rest of the data.
View(KenyaSHP |> st_drop_geometry())

# Shapefile Data Inspection
print(KenyaSHP[5:9], n = 6)
colnames(KenyaSHP)
class(KenyaSHP)

# Look at the variable data types
glimpse(KenyaSHP)

# View the geometry column
KenyaSHP_geometry <- st_geometry(KenyaSHP)
View(KenyaSHP_geometry)
# View one geometry column entry
View(KenyaSHP_geometry[[1]])
# View the classes of the geometry columns
class(KenyaSHP_geometry) #sfc, the list-column with the geometries for each feature
class(KenyaSHP_geometry[[1]]) #sfg, the feature geometry of an individual simple feature

# Change the projection of the shapefiles (if necessary)
KenyaSHP <- st_transform(KenyaSHP, crs = 4326)

# Inspect the co-ordinate reference system
st_crs(KenyaSHP)

# 6) Clean the data, so that the counties match those in the shapefile

# Inspect the county names in the male:female ratio dataset
df_1_ratio_only_county_unique <- unique(df_1_ratio_only_county$County)
df_1_ratio_only_county_unique

# Inspect the county names in the shapefile
counties_KenyaSHP <- KenyaSHP |> 
  st_drop_geometry() |> 
  select(COUNTY) |> 
  pull() |>
  unique()

counties_KenyaSHP

### Convert the m_f_ratio county names to title case
df_1_ratio_only_county <- df_1_ratio_only_county |> 
  ungroup() |> 
  mutate(County = tools::toTitleCase(tolower(County)))

### Inspect the county names of the m_f_ratio data again 
df_1_ratio_only_county_unique <- unique(df_1_ratio_only_county$County)


### Inspect the county names that are different in each of the datasets
unique(df_1_ratio_only_county$County)[which(!unique(df_1_ratio_only_county$County) %in% counties_KenyaSHP)]


df_1_ratio_only_county <- df_1_ratio_only_county |> 
  mutate(County = ifelse(County == "Taita/Taveta", "Taita Taveta",
                         ifelse(County == "Tharaka-Nithi", "Tharaka",
                                ifelse(County == "Elgeyo/Marakwet", "Keiyo-Marakwet",
                                       ifelse(County == "Nairobi City", "Nairobi", County)))))

# Check again for unique datasets
unique(df_1_ratio_only_county$County)[which(!unique(df_1_ratio_only_county$County) %in% counties_KenyaSHP)]

# 7) Join the shapefile and the data

### Rename the COUNTY variable, to match the variable name in the shapefile data
df_1_ratio_only_county <- df_1_ratio_only_county |> 
  rename(COUNTY = County)

### Ensure that there are no leading or trailing spaces in the county variable
KenyaSHP$COUNTY <- trimws(KenyaSHP$COUNTY)
df_1_ratio_only_county$COUNTY <- trimws(df_1_ratio_only_county$COUNTY)

### Merge the data
merged_df <- left_join(KenyaSHP, df_1_ratio_only_county, by = "COUNTY")

### Sort the data so that the County variable appears first
merged_df <- merged_df |> 
  select(COUNTY, everything())


# 8) Inspect the merged data

# View the data
View(merged_df)
View(merged_df |> st_drop_geometry())

### Class of the merged data
class(merged_df)

### Column names
colnames(merged_df)

# Glimpse
glimpse(merged_df)
