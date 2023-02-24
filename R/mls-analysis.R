# Created 2021-05-18 ----

library(tidyverse)
library(lubridate)
library(mapbaltimore)

area_boundaries <- getdata::get_location(
  mapbaltimore::neighborhoods,
  c("Ellwood Park/Monument", "Baltimore Highlands")
  )

property_count_by_neighborhood <- readr::read_rds(file = "data/area_property_count_by_neighborhood.rda")

# Requires a cached copy of the real property data ----
real_property <- sfext::read_sf_pkg("real_property.gpkg", package = "mapbaltimore")

# Import MLS data ----
mls_df <- list.files("data", full.names = TRUE) %>%
  tibble(path = .) %>%
  filter(str_detect(path, "_Market")) %>%
  mutate(
    results = purrr::map(
      path,
      ~ readr::read_csv(.x, col_types = readr::cols(.default = "c"), trim_ws = TRUE)
    )
  ) %>%
  tidyr::unnest(results) %>%
  janitor::clean_names("snake") %>%
  mutate(
    # Convert path names into neighborhood names
    neighborhood_name = str_remove(str_remove(path, "_Market Analysis Export.csv$"), "^data/"),
    neighborhood_name = str_trim(neighborhood_name)
  ) %>%
  tidyr::replace_na(replace = list(street_direction = "")) %>%
  mutate(
    # Clean neighborhood names to match real property data (specific to area)
    neighborhood_name = case_when(
      neighborhood_name == "Ellwood Park" ~ "Ellwood Park/Monument",
      neighborhood_name == "Madison Eastend" ~ "Madison-Eastend",
      neighborhood_name == "Patterson Park" ~ "Patterson Park Neighborhood",
      TRUE ~ neighborhood_name
    ),
    # Clean and fill street name, street direction (specific to area)
    street_direction = case_when(
      str_detect(street_name, "^N\\.[:space:]|^N\\.|^N[:space:]|North[:space:]|[:space:]N$|[:space:]N\\.$") ~ "N",
      str_detect(street_name, "^E\\.[:space:]|^E[:space:]|[:space:]E$|^East[:space:]") ~ "E",
      str_detect(street_name, "^S\\.[:space:]|^S[:space:]|[:space:]S$|^South[:space:]") ~ "S",
      str_detect(street_name, "Baltimore|Lombard|Fayette|Pratt|Fairmount|Monument|Madison|Eager") ~ "E",
      TRUE ~ street_direction
    ),
    street_name = str_remove(
      street_name,
      "^N\\.[:space:]|^N\\.|^N[:space:]|[:space:]N$|[:space:]N\\.$|^North[:space:]|^E\\.[:space:]|^E[:space:]|[:space:]E$|^East[:space:]|^S\\.[:space:]|^S[:space:]|[:space:]S$|^South[:space:]"
    ),
    street_name = str_remove(
      street_name,
      "[:space:]Street$|[:space:]St$|[:space:]Ave$"
    ),
    street_direction = case_when(
      str_detect(path, "Ellwood|Madison") & str_detect(street_name, "Linwood|Curley|Potomac|Decker|Ellwood|Robinson|^East|Bouldin|Clinton|Highland") ~ "N",
      str_detect(path, "Highlandtown") & str_detect(street_name, "Linwood|Curley|Potomac|Decker|Ellwood|Robinson|^East|Bouldin|Clinton|Highland|Conkling|Dean|Eaton|Haven") ~ "S",
      str_detect(path, "Madison|McElderry|Patterson[:space:]Park") & str_detect(street_name, "Milton|Rose|Luzerne|Glover|Lakewood|Belnord|Kenwood|Streeper|Bradford|Montford|Port") ~ "N",
      TRUE ~ street_direction
    ),
    # Rename street number, direction, and name variables for matching to real property data
    strtnum = street_number,
    strtdir = street_direction,
    strtnam = str_to_upper(street_name),
    property_address = str_squish(paste(street_number, street_direction, street_name))
  ) %>%
  naniar::replace_with_na(replace = list(interior_sq_ft = c("0", "1"))) %>%
  mutate(
    # Clean MLS variables
    list_date = mdy(list_date),
    list_year = year(list_date),
    list_month = month(list_date),
    list_quarter = quarter(list_date, with_year = TRUE),
    settled_date = mdy(settled_date),
    settled_year = year(settled_date),
    settled_month = month(settled_date),
    settled_quarter = quarter(settled_date, with_year = TRUE),
    off_market_date = mdy(off_market_date),
    # Calculated days on market does not match provided days on market (dom) or cumulative days on market (cdom)
    # days_on_market = interval(list_date, settled_date) / ddays(1),
    original_price = parse_number(original_price),
    original_price_k = original_price / 1000,
    list_price = parse_number(list_price),
    list_price_k = list_price / 1000,
    sold_price = parse_number(sold_price),
    sold_price_k = sold_price / 1000,
    ratio_sold_list = sold_price / list_price,
    diff_list_sale_price = sold_price - list_price,
    # Impute sale type based on list office name and financing
    final_short_sale = case_when(
      (is.na(final_short_sale) && !str_detect(list_office_name, "Auction|Billig|REXTAR") && (final_financing == "Conventional")) ~ "StandardSale (Imputed)",
      (is.na(final_short_sale) && str_detect(list_office_name, "Auction|Billig|REXTAR")) ~ "Auction (Imputed)",
      TRUE ~ final_short_sale),
    financing_yn = if_else(str_detect(final_financing, "Cash"), "No", "Yes"),
    standard_sale_yn = if_else(str_detect(final_short_sale, "StandardSale"), "Yes", "No"),
    auction_sale_yn = if_else(str_detect(final_short_sale, "Auction"), "Yes", "No"),
    reo_yn = if_else(str_detect(final_short_sale, "BankOwnedREO"), "Yes", "No"),
    short_sale_yn = if_else(str_detect(final_short_sale, "ShortSale"), "Yes", "No"),
    tax_annual_total = as.numeric(tax_annual_total),
    block_number = floor(parse_number(street_number) / 100) * 100,
    dom = as.integer(dom),
    cdom = as.integer(cdom),
    interior_sq_ft = as.numeric(interior_sq_ft),
    per_sq_ft_price = sold_price / interior_sq_ft,
    # Remove outlying values for price per square foot based on incorrect interior sq ft data
    per_sq_ft_price = if_else(per_sq_ft_price > 250 | per_sq_ft_price < 5, 0, per_sq_ft_price)
  ) %>%
  naniar::replace_with_na(replace = list(per_sq_ft_price = 0)) %>%
  # Remove duplicate sales
  distinct(mls_number, .keep_all = TRUE) %>%
  # Remove unused columns
  select(-c(path, type, final_third_party_approval:final_bank_owned, additional_bank_owned:additional_short_sale, mls_area, township, school_district)) %>%
  # Requires property_count_by_neighborhood
  left_join(property_count_by_neighborhood, by = c("neighborhood_name" = "neighborhood")) %>%
  # Rename variables
  rename(
    sale_type = final_short_sale,
    neighborhood = neighborhood_name,
    annual_tax = tax_annual_total
  )


# Match cleaned MLS data to property data
# NOTE: Matching to real property by address will cause problems for condo units
mls_prop <- mls_df %>%
  left_join(real_property, by = c("strtnum", "strtdir", "strtnam")) %>%
  filter(!is.na(property_id)) %>%
  # Remove unmatched listings
  sf::st_as_sf() %>%
  sf::st_transform(2804)


gt_fmt <- function(x) {
  x %>%
    gt::gt() %>%
    gt::fmt_number(columns = ends_with("dom"), decimals = 0) %>%
    gt::fmt_number(columns = starts_with("ratio"), decimals = 2) %>%
    gt::fmt_number(columns = starts_with("num"), decimals = 0) %>%
    gt::fmt_percent(columns = starts_with("perc")) %>%
    gt::fmt_currency(columns = contains("price"), decimals = 0) %>%
    gt::fmt_currency(columns = ends_with("tax"), decimals = 0)
}

# from https://stackoverflow.com/questions/63498880/how-to-bin-values-into-positive-and-negative-using-the-r-package-scales
#  gt::data_color(
#    columns = vars(ratio_properties_sales_relative),
#    colors = scales::col_bin(
#      bins = c(-Inf, 0, Inf),
#      palette = c("red", "green"),
#    )
#  )

labelled::var_label(mls_df) <- list(
  sold_price_k = 'Sale price ($, thousands)',
     ratio_sold_list = 'Sale to list price ratio',
     per_sq_ft_price = 'Price per square foot',
     dom = 'Days on market',
     cdom = 'Cumulative days on market',
     financing_yn = 'Financing used',
     standard_sale_yn = 'Standard sale',
     neighborhood = 'Neighborhood')

# Table: gtsummary table of key variables
mls_df %>%
  select(
    sold_price_k,
    ratio_sold_list,
    per_sq_ft_price,
    # days_on_market,
    dom,
    cdom,
    # sale_type,
    financing_yn,
    standard_sale_yn,
    neighborhood
  ) %>%
  gtsummary::tbl_summary(by = neighborhood)


# Create a function for summarizing data by area
summarise_mls_area <- function(x) {
  x %>%
    summarise(
      num_sales = n(),
      num_addresses = n_distinct(property_address),
      num_listing_agents = n_distinct(list_agent_code),
      num_selling_agents = n_distinct(selling_agent_code),
      num_sales_financing = sum(financing_yn == "Yes", na.rm = TRUE),
      perc_sales_financing = num_sales_financing / num_sales,
      num_sales_cash = sum(financing_yn == "No", na.rm = TRUE),
      perc_sales_cash = num_sales_cash / num_sales,
      mean_dom = round(mean(dom), digits = 2),
      mean_cdom = round(mean(cdom), digits = 2),
      mean_list_price = mean(list_price),
      mean_sale_price = mean(sold_price),
      median_sale_price = median(sold_price),
      mean_ratio_sold_list = round(mean(ratio_sold_list), digits = 2),
      mean_annual_tax = mean(annual_tax, na.rm = TRUE),
      mean_per_sq_ft_price = mean(per_sq_ft_price, na.rm = TRUE),
      # Following summary variables are only available for records after 2018 Q4
      num_sales_auction = sum(auction_sale_yn == "Yes", na.rm = TRUE),
      perc_sales_auction = num_sales_auction / num_sales,
      num_reo = sum(reo_yn == "Yes", na.rm = TRUE),
      perc_reo = num_reo / num_sales,
      num_short_sales = sum(short_sale_yn == "Yes", na.rm = TRUE),
      perc_short_sales = num_short_sales / num_sales,
      num_sales_standard = sum(standard_sale_yn == "Yes", na.rm = TRUE),
      perc_sales_standard = num_sales_standard / num_sales,
      num_area_res_properties = unique(num_res_properties),
      ratio_properties_sales = num_area_res_properties / num_sales,
      ratio_properties_financing = num_area_res_properties / num_sales_financing
    )
}

mls_neighborhood <- mls_df %>%
  group_by(neighborhood) %>%
  summarise_mls_area()

mls_neighborhood_yr <- mls_df %>%
  group_by(neighborhood, settled_year) %>%
  summarise_mls_area() %>%
  mutate(
    ratio_properties_sales_relative = 15 - ratio_properties_sales
  )

mls_neighborhood_qrt <- mls_df %>%
  group_by(neighborhood, settled_quarter) %>%
  summarise_mls_area()

mls_neighborhood_financing_year <- mls_df %>%
  group_by(neighborhood, financing_yn, settled_year) %>%
  summarise_mls_area() # %>%
#  select(-c(num_sales_financing:perc_num_sales_cash))


focus_area <- c("Baltimore Highlands", "Ellwood Park/Monument")
exclude_area <- c("Kresson", "Orangeville")
highlight_color <- "darkorange"
source_caption <- "Source: Bright MLS/MRIS (2015-2020)"

theme_set(theme_minimal())

# Plots ----

mls_df %>%
  count(neighborhood) %>%
  arrange(desc(n)) # %>%

# Plot: Residential property sales by neighborhood ----
mls_neighborhood %>%
  ggplot() +
  geom_col(aes(x = reorder(neighborhood, num_sales), y = num_sales), fill = highlight_color, alpha = 0.8) +
  gghighlight::gghighlight(neighborhood %in% focus_area) +
#  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "Residential property sales by neighborhood (2015-2020)",
    x = "Neighborhood",
    y = "Total sales",
    caption = source_caption
  )

# Plot: Sales by per square foot price ----
mls_df %>%
  filter(!is.na(per_sq_ft_price)) %>%
  ggplot() +
  geom_histogram(
    aes(x = per_sq_ft_price),
    fill = highlight_color, binwidth = 10, alpha = 0.8) +
  geom_vline(
    data = filter(mls_neighborhood, neighborhood %in% focus_area),
    aes(xintercept = mean_per_sq_ft_price),
    linetype = "dashed") +
  ggrepel::geom_label_repel(
    data = filter(mls_neighborhood, neighborhood %in% focus_area),
    aes(x = mean_per_sq_ft_price, label = paste0("$", round(mean_per_sq_ft_price, digits = 0), " ppsf (", neighborhood, ")")),
    y = 100) +
  scale_x_continuous(labels = scales::label_dollar()) +
  gghighlight::gghighlight(neighborhood %in% focus_area) +
  labs(
    title = "Sales by price per square foot",
    subtitle = paste("Distribution and mean highlighted for", paste(focus_area, collapse = " & ")),
    x = "Price ($) per square foot",
    y = "Sales (#)",
    caption = paste(source_caption, "; outlying ppsf values (< 5 and > 250) excluded.")
  )

# Table (internal): Addresses sold 3 or more times
mls_df %>%
  group_by(neighborhood) %>%
  count(property_address) %>%
  filter(n >= 3) %>%
  arrange(desc(n)) %>%
  gt::gt()


# Plot: Sales and financed sales by neighborhood ----
mls_neighborhood_qrt %>%
  filter(!(neighborhood %in% exclude_area)) %>%
  ggplot(aes(x = yq(settled_quarter), fill = neighborhood %in% focus_area)) +
  geom_col(aes(y = num_sales), alpha = 0.6) +
  geom_col(aes(y = num_sales_financing), alpha = 0.6) +
  geom_smooth(aes(y = num_sales)) +
  scale_fill_brewer(palette = "Accent", direction = -1) +
  facet_wrap(~neighborhood) +
  guides(fill = "none") +
  labs(
    x = "Quarter",
    y = "Sales",
    caption = source_caption
  ) +
  theme_minimal()
  # gghighlight::gghighlight(neighborhood %in% focus_area)

# Plot: Percent cash sales ----
mls_neighborhood_yr %>%
  ggplot(aes(x = settled_year, y = perc_sales_cash, color = neighborhood)) +
  geom_point(size = 3) +
  geom_line(aes(group = neighborhood), size = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  gghighlight::gghighlight(neighborhood %in% focus_area) +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "Year",
    y = "Cash sales (% of of total sales)",
    color = "Neighborhood",
    caption = source_caption
  )

# Table: Summary of cash sales by neighborhood and year ----
mls_df %>%
  filter(financing_yn == "No") %>%
  group_by(neighborhood, settled_year) %>%
  summarise_mls_area() %>%
  select(-c(mean_cdom, mean_list_price, median_sale_price, num_addresses:num_selling_agents, num_sales_financing, perc_sales_financing, num_sales_cash, perc_sales_cash, mean_per_sq_ft_price:perc_sales_standard)) %>%
  gt_fmt()



mls_neighborhood_yr %>%
  #  filter(neighborhood_name == "Ellwood Park/Monument") %>%
  gt_fmt()

# Map (internal - suggest removal): Standard vs. non-standard sales ----
mls_prop %>%
  filter(!is.na(standard_sale_yn)) %>%
  ggplot(aes(color = standard_sale_yn)) +
  geom_sf(size = 3, alpha = 0.4) +
  #geom_sf(data = area_boundaries, fill = NA, color = "gray40") +
  scale_color_viridis_d() +
  theme_void() +
  labs(
    caption = paste(source_caption, "; mapped sales are a subset of all sales.")
  )

# Map: Sales with financing ----
mls_prop %>%
  filter(!is.na(financing_yn)) %>%
  mutate(
    financing_yn = case_when(financing_yn == "No" ~ "Cash sale",
                             financing_yn == "Yes" ~ "Non-cash sale")
  ) %>%
  ggplot() +
  geom_sf(data = streets, color = "gray60") +
  geom_sf(aes(fill = financing_yn), color = NA, alpha = 0.8) +
  geom_sf(data = parks, fill = "forestgreen", alpha = 0.4, color = NA) +
  geom_sf(data = neighborhoods, fill = NA, color = "gray40") +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(
    color = ""
  ) +
  set_map_limits(area = area_boundaries)

label_every_other <- function(x) {
  x <- sort(unique(x))
  x[seq(2, length(x), 2)] <- ""
  x
}

# Plot: Median sales price per quarter ----
mls_neighborhood_qrt %>%
  ggplot(aes(x = yq(settled_quarter), y = median_sale_price, fill = neighborhood)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~neighborhood) +
  scale_y_continuous(labels = scales::dollar_format()) +
  #scale_x_discrete(labels = label_every_other((mls_neighborhood_yr$settled_quarter))) +
 # scale_fill_viridis_d() +
  guides(fill = "none") +
  labs(
    x = "Settlement quarter",
    y = "Median sale price"
  ) +
  theme_minimal()


# Plot: Financed sales by neighborhood and year ----
mls_df %>%
  filter(
    #sold_price_k < 1000,
    ratio_sold_list > 0.125,
    neighborhood != "Orangeville"
  ) %>%
  filter(financing_yn == "Yes") %>%
  ggplot(aes(x = settled_date, y = sold_price_k, color = neighborhood)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  gghighlight::gghighlight(neighborhood %in% focus_area) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Date settled",
    y = "Price sold ($ thousands)",
    caption = "Sales in Orangeville and sales with outlying sold/list ratio <= 0.125 excluded."
  )
  #  scale_y_log10()#  +
  facet_wrap(~neighborhood)


mls_df %>%
  mutate(
    street_type = if_else(
      block_number > 2000,
      "EW",
      "NS"
    )
  ) %>%
  filter(
    sold_price_k < 1000,
    ratio_sold_list > 0.125,
    neighborhood != "Orangeville"
  ) %>%
  #  filter(financing_yn == "Yes") %>%
  filter(street_type == "EW") %>%
  ggplot(aes(x = paste0(street_direction, block_number), y = sold_price_k, color = neighborhood)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  scale_color_brewer(palette = "Dark2") +
  #  scale_y_log10() +
  facet_wrap(~street_type)

mls_df %>%
  filter(str_detect(neighborhood, "Ellwood")) %>%
  ggplot() +
  geom_bar(aes(x = settled_quarter))

mls_prop %>%
  filter(
    sold_price_k < 500,
    ratio_sold_list > 0.125,
    neighborhood.y != "Orangeville"
  ) %>%
  ggplot() +
  geom_sf(aes(fill = sold_price_k), alpha = 0.7, color = NA) +
  scale_fill_viridis_c() +
  set_map_limits(area = area_boundaries) +
  theme_void()
