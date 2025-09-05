# GEOM HEX map of Kenya
# William Okech

# Install if missing:
# install.packages(c("sf","terra","geodata","dplyr","ggplot2"))
# install.packages("geogrid") # from CRAN

library(sf)
library(terra)
library(geodata)
library(dplyr)
library(ggplot2)
library(geogrid)

# --- 1) Get Kenya counties (GADM level 1) ---
ken_l1 <- geodata::gadm(country = "KEN", level = 1, path = "sub_pro_1_kenya_county_sub_county")
ken_counties <- sf::st_as_sf(ken_l1)

# --- 2) Pick a name column robustly ---
# GADM typically has "NAME_1", but schema may vary
name_col <- grep("^NAME", names(ken_counties), value = TRUE)[1]
ken_counties <- ken_counties |>
  dplyr::select(county = all_of(name_col)) |>
  sf::st_make_valid()

# --- 3) Reproject for correct area ---
ken_counties <- st_transform(ken_counties, 3857)

# Compute area in km² as fallback variable
ken_counties <- ken_counties |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)

# --- 4) (Optional) Join your own data ---
# Example: suppose you have a data frame of county-level values
# Replace this with your real dataset
mydata <- tibble::tibble(
  county = c("Nairobi", "Mombasa", "Kisumu", "Nakuru"),
  population_millions = c(4.4, 1.2, 1.1, 2.2)
)

# Join by county name (make sure names match GADM spelling!)
ken_counties <- ken_counties |>
  left_join(mydata, by = "county")

# If some counties don’t match, you’ll get NA — check with:
# anti_join(mydata, ken_counties, by = "county")

# --- 5) Build a contiguous hex cartogram ---
set.seed(42)
hex_grid <- calculate_grid(
  ken_counties,
  learning_rate = 0.2,
  grid_type = "hexagonal"
)

ken_hex <- assign_polygons(ken_counties, hex_grid)

# --- 6) Plot hex cartogram with your variable ---
ggplot(ken_hex) +
  geom_sf(aes(fill = population_millions), color = "white", linewidth = 0.3) +
  coord_sf() +
  scale_fill_viridis_c(option = "C", name = "Population (millions)", na.value = "grey90") +
  labs(
    title = "Kenya Counties — Hex Cartogram",
    subtitle = "One hex per county (adjacency-preserving layout)",
    caption = "Boundaries: GADM via {geodata} | Demo data"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

# --- 7) Compare to real counties map (optional) ---
ggplot(ken_counties) +
  geom_sf(aes(fill = population_millions), color = "white", linewidth = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(option = "C", name = "Population (millions)", na.value = "grey90") +
  labs(
    title = "Kenya Counties — Actual Geography",
    caption = "Boundaries: GADM via {geodata} | Demo data"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank())
