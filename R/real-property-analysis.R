# Created 2021-05-17 ----

library(tidyverse)
library(lubridate)
library(mapbaltimore)

# What is the area of analysis? ----

area_name <- c("Baltimore Highlands",
               "Ellwood Park/Monument",
               "Highlandtown",
               "Kresson",
               "Madison-Eastend",
               "McElderry Park",
               "Orangeville",
               "Patterson Park Neighborhood")

area <- get_area("neighborhood", area_name, union = TRUE)

area_boundaries <- get_area("neighborhood", area_name, union = FALSE)

area_boundary_layer <-
  list(
    geom_sf(data = area_boundaries, fill = NA, color = "gray30"),
    theme_void()
  )

# Import area property data
# area_property <- get_area_property(area = area, dist = 50)
# cache_baltimore_data(data = area_property, filename = "secdc_plan_property_data.gpkg")
area_property <-
  get_area_data(area = area, dist = 50, crop = FALSE, data = "secdc_plan_property_data")

area_zip <- c("21224", "21205", "21213", "21202", "21231")
# From Greg Preston (October 21, 2020) https://twitter.com/gregexpr/status/1319067274863542273
llc_regex <- "\\s(LL(P|S)|L\\s?P|L\\s?L\\s?C|INC|L\\s?M?\\s?T\\s?D|LIMITED|(IN)?CORP([A-Z]+)?)\\b.*$"

area_property <- area_property %>%
  mutate(
    # Create a owner name full display column and owner key for joins
    #owner_full = paste(ownname1, ownname2),
    owner_full = paste(owner_1, owner_2, owner_3),
    owner_key = str_remove_all(str_to_lower(owner_full), "[:punct:]|[:space:]"),
    # Recode w/ LLC and local owner flag
    # local_owner_flag = if_else((ownerzip %in% area_zip) & (owncity == "BALTIMORE"), "Yes", "No"),
    llc_flag = if_else(str_detect(owner_full, llc_regex), "Yes", "No"),
    block_number = floor(bldg_num / 100) * 100,
    even_odd = if_else((bldg_num %% 2) == 0, "Even", "Odd"),
    block_face_st = paste0(even_odd, block_number, street_dirpre, street_name, street_type, sep = " "),
    block_number_st = paste0(block_number, street_dirpre, street_name, street_type, sep = " ")
  )

property_count_by_neighborhood <- area_property %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(neighborhood)) %>%
  group_by(neighborhood) %>%
  summarise(
    num_properties = n(),
    num_res_properties = sum(resityp %in% c("SF", "AP", "CN")),
    perc_area_res_properties = num_res_properties / num_properties
  )

readr::write_rds(property_count_by_neighborhood, file = "area_property_count_by_neighborhood.rda")

area_property <- area_property %>%
  left_join(property_count_by_neighborhood, by = "neighborhood")

# What property owners hold large numbers of properties in the area? ----

area_property_owner_count <- area_property %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(neighborhood),
         resityp %in% c("SF", "AP", "CN")) %>%
  group_by(neighborhood) %>%
  count(owner_key) %>%
  arrange(desc(n)) %>%
  left_join(
    distinct(select(sf::st_drop_geometry(area_property), ownnamefull, owner_key), owner_key, .keep_all = TRUE),
    by = "owner_key") %>%
  ungroup()


n_threshold <- 5

#select_area_name <- "Ellwood Park/Monument"
#select_area <- filter(area_boundaries, name == select_neighborhood)

large_property_owners <- area_property %>%
  # sf::st_drop_geometry() %>%
  # filter(neighborhood == select_area_name) %>%
  left_join(select(area_property_owner_count, n, owner_key, neighborhood),
            by = c("owner_key", "neighborhood")) %>%
  filter(!is.na(n),
         n >= n_threshold)


# Table: Large scale property owners for select neighborhood ----
large_property_owner_table <- large_property_owners %>%
  group_by(owner_key) %>%
  summarise(
    namekey = first(namekey),
    n = first(n),
    geometry = sf::st_union(geometry)
  ) %>%
  #distinct(owner_key, .keep_all = TRUE) %>%
  arrange(desc(n))

large_property_owner_table %>%
  sf::st_drop_geometry() %>%
  select(namekey, n) %>%
  gt::gt() %>%
  gt::summary_rows(
    columns = "n",
    fns = list(total = "sum")
  )


large_property_owner_table %>%
  ggplot() +
  geom_sf(aes(color = n), alpha = 0.6, size = 2) +
  #  geom_mark_hull(
  #    data = filter(large_property_owners, owner_key == "summerfieldinvestmentgroupllc"), # Largest property owner in Ellwood Park
  #    aes(label = namekey, geometry = geometry),
  #    stat = "sf_coordinates"
  #    ) +
  geom_mark_hull(
    data = filter(large_property_owners, owner_key == "nopallc"), # Largest property owner in Baltimore Highlands
    aes(label = namekey, geometry = geometry),
    stat = "sf_coordinates"
  ) +
  geom_mark_hull(
    data = filter(large_property_owners, owner_key == "kfrnallc"), # Largest property owner in Baltimore Highlands
    aes(label = namekey, geometry = geometry),
    stat = "sf_coordinates"
  ) +
  geom_mark_hull(
    data = filter(large_property_owners, owner_key == "harrisjeffrey"), # 2nd largest property owner in Ellwood Park
    aes(label = namekey, geometry = geometry),
    stat = "sf_coordinates"
  ) +
  scale_color_viridis_c(option = "A",direction = -1, end = 0.8) +
  area_boundary_layer #+
# set_map_limits(area = select_area)

neighborhood_comparison_property_table <- area_property %>%
  filter(!is.na(neighborhood)) %>%
  group_by(neighborhood) %>%
  summarise(
    num = n(),
    num_res = sum(resityp %in% c("SF", "AP", "CN")),
    perc_res = num_res / num,
    num_large_owners = sum(owner_key %in% large_property_owners$owner_key),
    perc_large_owners = num_large_owners / num_res,
    num_res_llc = sum((resityp %in% c("SF", "AP", "CN")) & (llc_flag == "Yes")),
    perc_res_llc = num_res_llc / num_res
  ) %>%
  arrange(perc_res_llc) %>%
  sf::st_drop_geometry() %>%
  gt::gt() %>%
  gt::fmt_percent(columns = starts_with("perc")) %>%
  gt::fmt_number(columns = starts_with("num"), decimals = 0)

gt::gtsave(neighborhood_comparison_property_table, filename = "neighborhood_comparison_property_table.png")

map_theme <- theme_minimal() +
  theme(
    panel.grid.major = ggplot2::element_line(color = "transparent"),
    axis.title = ggplot2::element_text(color = "transparent"),
    axis.text = ggplot2::element_text(color = "transparent"),
    legend.position = "bottom")


# Map: LLC owned flag ----
ggplot() +
  snapbox::layer_mapbox(area = adjust_bbox(area_boundaries, asp = "1:1"),
                        map_style = snapbox::mapbox_light(),
                        mapbox_api_access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN")) +
  geom_sf(data = filter(area_property, resityp %in% c("SF", "AP", "CN")), aes(color = llc_flag), alpha = 0.5) +
  scale_color_viridis_d(begin = 0.1) +
  map_theme +
  labs(color = "Property LLC-owned")

# Map: Assessed value ----
assessed_value_map <-
  ggplot() +
  snapbox::layer_mapbox(area = area_boundaries,
                        map_style = snapbox::mapbox_light(),
                        mapbox_api_access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN")) +
  geom_sf(data = filter(area_property,
                        resityp %in% c("SF", "AP", "CN"),
                        nfmttlvl <= 500000,
                        nfmttlvl >= 10000),
          aes(color = nfmttlvl), alpha = 0.5) +
  scale_color_viridis_c(begin = 0.1, labels = scales::label_dollar(), trans = scales::log10_trans()) +
  map_theme +
  labs(color = "Asessed value") # +
#  mapbaltimore::set_map_limits(area = area_boundaries[area_boundaries$name == "Ellwood Park/Monument",], dist = 100)

assessed_value_map +
  theme(legend.key.width=unit(4,"cm"))


