# Map variables --------------------------------------------------------------
louisiana <- opq(getbb("Louisiana"), timeout = 60 * 20) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "admin_level", value = c("4", "6", "8")) |>
  osmdata_sf()
louisiana$osm_multipolygons <- st_make_valid(louisiana$osm_multipolygons)
state <- subset(louisiana$osm_multipolygons,
                admin_level == "4" & name == "Louisiana")
parishPolygons <- louisiana$osm_multipolygons |>
  st_filter(state, .predicate=st_within) |>
  subset(admin_level == 6)
acadiana <- subset(parishPolygons,
  name == parishes[1] | name == parishes[2] | name == parishes[3] |
  name == parishes[4] | name == parishes[5] | name == parishes[6] |
  name == parishes[7] | name == parishes[8] | name == parishes[9] |
  name == parishes[10] | name == parishes[11] | name == parishes[12] |
  name == parishes[13] | name == parishes[14] | name == parishes[15] |
  name == parishes[16] | name == parishes[17] | name == parishes[18] |
  name == parishes[19] | name == parishes[20] |
  name == parishes[21] | name == parishes[22])
acadianaParishCenters <- getPolyCentersFromPlaces(parishes)
acadianaParishCenters[acadianaParishCenters$place == "Calcasieu Parish", "latitude"] <- acadianaParishCenters[acadianaParishCenters$place == "Calcasieu Parish", "latitude"] - 0.05
acadianaParishCenters[acadianaParishCenters$place == "Jefferson Davis Parish", "latitude"] <- acadianaParishCenters[acadianaParishCenters$place == "Jefferson Davis Parish", "latitude"] + 0.05
acadianaParishCenters[acadianaParishCenters$place == "St. Landry Parish", "longitude"] <- acadianaParishCenters[acadianaParishCenters$place == "St. Landry Parish", "longitude"] + 0.085
acadianaParishCenters[acadianaParishCenters$place == "Pointe Coupee Parish", "latitude"] <- acadianaParishCenters[acadianaParishCenters$place == "Pointe Coupee Parish", "latitude"] - 0.05
acadianaParishCenters[acadianaParishCenters$place == "Lafourche Parish", "longitude"] <- acadianaParishCenters[acadianaParishCenters$place == "Lafourche Parish", "longitude"] + 0.15
acadianaParishCenters[acadianaParishCenters$place == "Saint Martin Parish", "latitude"] <- acadianaParishCenters[acadianaParishCenters$place == "Saint Martin Parish", "latitude"] + 0.225
acadianaParishCenters[acadianaParishCenters$place == "Saint Martin Parish", "longitude"] <- acadianaParishCenters[acadianaParishCenters$place == "Saint Martin Parish", "longitude"] - 0.1
acadianaParishCenters[acadianaParishCenters$place == "Iberia Parish", "latitude"] <- acadianaParishCenters[acadianaParishCenters$place == "Iberia Parish", "latitude"] + 0.15
acadianaParishCenters[acadianaParishCenters$place == "Iberia Parish", "longitude"] <- acadianaParishCenters[acadianaParishCenters$place == "Iberia Parish", "longitude"] - 0.1

participant_raised <- subset(parishPolygons,
  name == "Calcasieu Parish" | name == "Cameron Parish" | name == "Vermilion Parish" |
  name == "Acadia Parish" | name == "Evangeline Parish" | name == "Avoyelles Parish" |
  name == "St. Landry Parish" | name == "Lafayette Parish" | name == "Saint Martin Parish" |
  name == "Lafourche Parish")
participant_residence <- subset(parishPolygons,
  name == "Vermilion Parish" | name == "Acadia Parish" | name == "St. Landry Parish" |
  name == "Lafayette Parish" | name == "Saint Martin Parish" | name == "East Baton Rouge Parish")
focus_area <- subset(parishPolygons,
  name == "St. Landry Parish" | name == "Lafayette Parish" | name == "Saint Martin Parish")
majorCitiesPolygons <- subset(louisiana$osm_multipolygons, admin_level == "8" &
                              name == majorCities[1] | name == majorCities[2] | name == majorCities[3])
majorCitiesCenters <- getPolyCentersFromPlaces(majorCities)

# Maps ------------------------------------------------------------------------
mapla <- ggplot() +
  geom_sf(data = state) +
  geom_sf(data = parishPolygons) +
  theme_void()

cities <- geom_text(
  data = majorCitiesPolygons,
  aes(label = majorCities),
  y = majorCitiesCenters$latitude, x = majorCitiesCenters$longitude,
  size = 4, fontface = "bold")

mapacadiana <- mapla +
  geom_sf(data = acadiana,
          fill = color_key[2]) +
  geom_text(aes(label = parishes),
    y = acadianaParishCenters$latitude,
    x = acadianaParishCenters$longitude,
    size = 2) +
  theme_void()

mapacadianacities <- mapla +
  geom_sf(data = acadiana,
          fill = color_key[2]) +
  geom_sf(data = majorCitiesPolygons,
          fill = color_key[5]) +
  cities +
  theme_void()

mapraised <- mapla +
  geom_sf(data = acadiana,
          fill = color_key[2]) +
  geom_sf(data = participant_raised,
          fill = color_key[3]) +
  geom_sf(data = majorCitiesPolygons,
          fill = color_key[5]) +
  cities

mapraisedonly <- mapla +
  geom_sf(data = participant_raised,
          fill = color_key[3])

mapresidence <- mapla +
  geom_sf(data = acadiana,
          fill = color_key[2]) +
  geom_sf(data = participant_raised,
          fill = color_key[3]) +
  geom_sf(data = participant_residence,
          fill = color_key[1]) +
  geom_sf(data = majorCitiesPolygons,
          fill = color_key[5]) +
  cities

mapresidenceonly <- mapla +
  geom_sf(data = participant_residence,
          fill = color_key[1])

mapfocus <- mapla +
  geom_sf(data = focus_area,
          fill = color_key[4])
