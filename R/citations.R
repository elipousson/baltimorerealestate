where <- "Neighborhood = 'Baltimore Highlands'"

url <- "https://opendata.baltimorecity.gov/egis/rest/services/NonSpatialTables/Part1_Crime/FeatureServer/0"

crime <- get_esri_data(url = url, is_table = TRUE, location_col = "geo_location", where = where)

url <- "https://opendata.baltimorecity.gov/egis/rest/services/NonSpatialTables/ECB/FeatureServer/0"

citations <- get_esri_data(url = url, where = where, is_table = TRUE, location_col = "location")

description_count <- citations %>%
  sf::st_drop_geometry() %>%
  count(description) %>%
  arrange(desc(n))

annual_registration_citations <- citations %>%
  filter(
    lubridate::year(violation_date) >= 2015,
    description == "FAILURE TO FILE A COMPLETED ANNUAL REGISTRATION"
  ) %>%
  mutate(
    property_address = str_remove(violation_location, "^0+")
  ) %>%
  left_join(st_drop_geometry(area_property), by = "property_address")

