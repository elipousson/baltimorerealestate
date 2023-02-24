# Created 2021-05-18 ----

library(tidyverse)

# permits_path <- # Requires path to permit shapefile

permits <- sf::read_sf(permits_path) %>%
  janitor::clean_names("snake") %>%
  rename(
    description = descriptio,
    expiration_date = expiration,
    proposed_use = proposed_us,
    neighborhood = neighborho,
    council_district = council_di,
    cluster = housing_mar
  ) %>%
  mutate(
    issued_year = year(issued_date),
    issued_quarter = quarter(issued_date, with_year = TRUE),
    .after = issued_date
  )

permits <- permits %>%
  sf::st_transform(2804) %>%
  rename(neighborhood_orig = neighborhood) %>%
  sf::st_join(
    neighborhoods
  )

permits <- permits %>%
  rename(
    neighborhood = neighborhood_orig,
    neighborhood_name = name
  )

area_name <- c("Baltimore Highlands",
               "Ellwood Park/Monument",
               "Highlandtown",
               "Kresson",
               "Madison-Eastend",
               "McElderry Park",
               "Orangeville",
               "Patterson Park Neighborhood",
               "Baltimore-Linwood")


select_area_name <- area_name[1:2]

area_permits <- permits %>%
  filter(neighborhood %in% str_to_upper(area_name)) %>%
  mutate(
    neighborhood = case_when(
      neighborhood == "BALTIMORE-LINWOOD" ~ "PATTERSON PARK",
      TRUE ~ neighborhood
    ),
    neighborhood_match = case_when(
    neighborhood == "BALTIMORE HIGHLANDS" ~ "Baltimore Highlands",
    neighborhood == "ELLWOOD PARK/MONUMENT" ~ "Ellwood Park/Monument",
    neighborhood == "HIGHLANDTOWN" ~ "Highlandtown",
    neighborhood == "KRESSON" ~ "Kresson",
    neighborhood == "MADISON-EASTEND" ~ "Madison-Eastend",
    neighborhood == "MCELDERRY PARK" ~  "McElderry Park",
    neighborhood == "ORANGEVILLE" ~  "Orangeville",
    neighborhood == "PATTERSON PARK" ~  "Patterson Park Neighborhood"
    )
  )

area_permits %>%
  group_by(neighborhood, issued_quarter) %>%
  summarise(
    num_permits = n(),
    num_properties = n_distinct(blocklot)
  ) %>%
  ggplot(aes(y = num_properties, x = issued_quarter, color = neighborhood)) +
  geom_point(alpha = 0.8) +
  geom_smooth() +
  facet_wrap(~ neighborhood) +
  theme_minimal()

path_name <- utils::URLencode(str_to_upper(select_area$name))

permits_path <- paste0("https://opendata.arcgis.com/datasets/9f235fe8513d4cabbfb7de329af91e83_3.geojson?where=Neighborhood%20%3D%20'",path_name,"'")

  #esri2sf::esri2sf(permits_path, bbox = sf::st_bbox(sf::st_transform(area_boundaries, 2248)))

plot_permits_year <- function(x) {
  permit_plot <- ggplot() +
#    geom_bar(data = area_permits, aes(x = issued_year), alpha = 0.6) +
    geom_bar(data = filter(area_permits, neighborhood == x), aes(x = issued_year), fill = "darkblue") +
    geom_hline(yintercept = 239) + # Average permits per year across all neighborhoods
    theme_void()

  gt::ggplot_image(permit_plot)
}

area_permits %>%
  sf::st_drop_geometry() %>%
  left_join(area_property_count, by = c("neighborhood_match" = "neighborhood")) %>%
  group_by(neighborhood) %>%
  summarise(
    num_permits = n(),
    num_properties = n_distinct(address),
    perc_properties_permits = num_properties / unique(num_area_properties),
    mean_cost = mean(cost, na.rm = TRUE),
    median_cost = median(cost, na.rm = TRUE),
    num_permits_2015 = sum(issued_year == min(issued_year)),
    num_permits_2019 = sum(issued_year == (max(issued_year) - 1)),
    num_permits_2020 = sum(issued_year == max(issued_year)),
    perc_change_permits_2015_2019 = (num_permits_2019 / num_permits_2015) - 1,
    perc_change_permits_2015_2020 = (num_permits_2020 / num_permits_2015) - 1
  ) %>%
  ungroup() %>%
  mutate(
    plot_permits_year = neighborhood,
    .after = median_cost
  ) %>%
  gt::gt() %>%
  gt::fmt_percent(columns = starts_with("perc")) %>%
  gt::fmt_currency(columns = ends_with("cost"), decimals = 2) %>%
  gt::text_transform(
    locations = cells_body(vars(plot_permits_year)),
    fn = function(x) {
      purrr::map(x, ~ plot_permits_year(.x))
    }
  ) %>%
  gt::cols_label(
    neighborhood = "Neighborhood",
    num_permits = "Permits (#)",
    num_properties = "Properties (#)",
    perc_properties_permits = "% of properties w/ permits",
    mean_cost = "Avg. $",
    median_cost = "Median $",
    plot_permits_year = "2015-2019",
    num_permits_2015 = "2015",
    num_permits_2019 = "2019",
    num_permits_2020 = "2020",
    perc_change_permits_2015_2019 = "% change (15-19)",
    perc_change_permits_2015_2020 = "% change (15-20)"
  ) %>%
  gt::tab_spanner(
    label = "Overall",
    columns = vars(num_permits,num_properties, perc_properties_permits, mean_cost, median_cost)
  ) %>%
    gt::tab_spanner(
      label = "Permits by year issued",
      columns = vars(plot_permits_year,num_permits_2015, num_permits_2019, perc_change_permits_2015_2019, num_permits_2020, perc_change_permits_2015_2020)
    )
