library(dplyr)
library(getdata)
library(ggplot2)

url <- "https://geodata.baltimorecity.gov/egis/rest/services/Housing/codiDynamics/MapServer/26"

foreclosures <-
  get_esri_data(url = url, bbox = sf::st_bbox(sf::st_transform(area_boundaries, 2248)))

foreclosures_data <- foreclosures  %>%
  sf::st_transform(2804) %>%
  mutate(
    year = lubridate::year(date)
  ) %>%
  filter(
    year >= 2010
  ) %>%
  sf::st_join(select(area_boundaries, neighborhood = name)) %>%
  filter(!is.na(neighborhood)) %>%
  left_join(area_property_count, by = "neighborhood") %>%
  group_by(neighborhood, year) %>%
  summarise(
    foreclosures = n(),
    perc_properties_foreclosure = foreclosures / unique(num_area_properties)
  )
foreclosures_df <- foreclosures_data %>%
  sf::st_drop_geometry()

foreclosures_df %>%
  gt::gt() %>%
  gt::fmt_percent(starts_with("perc")) %>%
  gt::summary_rows(
    groups = TRUE,
    columns = vars(foreclosures, perc_properties_foreclosure),
    fns = list(
      min = ~min(.),
      max = ~max(.),
      avg = ~mean(.)
      )
  )

foreclosures_df %>%
  ggplot(aes(x = as.factor(year), y = perc_foreclosures, color = neighborhood)) +
  geom_point() + #, size = 2, alpha = 0.6)
  geom_smooth(aes(linetype = neighborhood)) +
  scale_color_viridis_d(direction = -1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal()
  `#facet_wrap(~ neighborhood)
