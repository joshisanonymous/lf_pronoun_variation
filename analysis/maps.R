# Packages -------------------------------------------------------------------
library(osmdata)
library(sf)

# Map variables --------------------------------------------------------------
# Louisiana
louisiana <- opq(getbb("Louisiana"), timeout = 60 * 20) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "admin_level", value = c("4", "6", "8")) |>
  osmdata_sf()
state <- subset(louisiana$osm_multipolygons,
                admin_level == "4" & name == "Louisiana")
parishes <- louisiana$osm_multipolygons |>
  st_filter(state, .predicate=st_within) |>
  subset(admin_level == 6)
acadiana <- subset(parishes,
  name == "Calcasieu Parish" | name == "Cameron Parish" | name == "Jefferson Davis Parish" |
  name == "Vermilion Parish" | name == "Acadia Parish" | name == "Evangeline Parish" |
  name == "Avoyelles Parish" | name == "St. Landry Parish" | name == "Lafayette Parish" |
  name == "Saint Martin Parish" | name == "Iberia Parish" | name == "St. Mary Parish" |
  name == "Assumption Parish" | name == "Iberville Parish" | name == "Pointe Coupee Parish" |
  name == "West Baton Rouge Parish" | name == "Ascension Parish" | name == "St. James Parish" |
  name == "St. John the Baptist Parish" | name == "St. Charles Parish" |
  name == "Lafourche Parish" | name == "Terrebonne Parish")
participant_raised <- subset(parishes,
  name == "Calcasieu Parish" | name == "Cameron Parish" | name == "Vermilion Parish" |
  name == "Acadia Parish" | name == "Evangeline Parish" | name == "Avoyelles Parish" |
  name == "St. Landry Parish" | name == "Lafayette Parish" | name == "Saint Martin Parish" |
  name == "Lafourche Parish")
participant_residence <- subset(parishes,
  name == "Vermilion Parish" | name == "Acadia Parish" | name == "St. Landry Parish" |
  name == "Lafayette Parish" | name == "Saint Martin Parish")
major_cities <- subset(louisiana$osm_multipolygons, admin_level == "8" &
                       name == "Lafayette" | name == "New Orleans" | name == "Baton Rouge")
major_cities_coords <- data.frame(
  "x" = c(-89.62518, -91.97133, -90.99934),
  "y" = c(30.19947, 30.29646, 30.55898)
)

# Maps ------------------------------------------------------------------------
mapla <- ggplot() +
  geom_sf(data = state) +
  geom_sf(data = parishes) +
  theme_void()

cities <- geom_text(
  data = major_cities,
  aes(label = name),
  y = major_cities_coords$y + 0.05, x = major_cities_coords$x + 0.175,
  size = 5, fontface = "bold")

mapacadiana <- mapla +
  geom_sf(data = acadiana,
          fill = color_key[2]) +
  geom_sf(data = major_cities,
          fill = color_key[5]) +
  cities +
  theme_void()

mapraised <- mapla +
  geom_sf(data = acadiana,
          fill = color_key[2]) +
  geom_sf(data = participant_raised,
          fill = color_key[3]) +
  geom_sf(data = major_cities,
          fill = color_key[5]) +
  cities

mapresidence <- mapla +
  geom_sf(data = acadiana,
          fill = color_key[2]) +
  geom_sf(data = participant_raised,
          fill = color_key[3]) +
  geom_sf(data = participant_residence,
          fill = color_key[1]) +
  geom_sf(data = major_cities,
          fill = color_key[5]) +
  cities