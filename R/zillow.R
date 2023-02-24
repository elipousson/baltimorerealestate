library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

zori_zip <- readr::read_csv("data-raw/Zip_ZORI_AllHomesPlusMultifamily_SSA.csv") %>%
  janitor::clean_names("snake") %>%
  filter(msa_name == "Baltimore, MD") %>%
  pivot_longer(cols = c(5:91),
               names_pattern = "x(.+)_(.+)",
               names_to = c("year", "month"),
               values_to = "zori") %>%
  mutate(
    date = ymd(paste(year, month, "01", sep = "-"))
  )

region_name_list <- c("21224")#, "21205")

zori_zip %>%
  ggplot(aes(x = date, y = zori, color = region_name)) +
  geom_point(alpha = 0.3, size = 1.5) +
  gghighlight::gghighlight(region_name %in% region_name_list) +
  scale_color_viridis_d() +
  labs(
    title = "Zillow ZORI All Homes Plus Multifamily price index for Baltimore MSA (21224 highlighted)"
  )

nhood_zhvi <- readr::read_csv("data/Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv") %>%
  janitor::clean_names("snake") %>%
  filter(state == "MD",
         city == "Baltimore") %>%
  pivot_longer(cols = c(10:312),
               names_to = c("year", "month", "day"),
               names_pattern = "x(.+)_(.+)_(.+)",
               values_to = "zhvi") %>%
  mutate(
    date = ymd(paste(year, month, day, sep = "-"))
  ) %>%
  filter(!is.na(zhvi))

region_name_list <- c("Ellwood Park-Monument",
                      "Highlandtown",
                      "Patterson Park",
                      "Madison - Eastend",
                      "McElderry Park",
                      "Greektown")

area_nhood_zhvi <- nhood_zhvi %>%
  filter(region_name %in% region_name_list,
         year >= 2000)

ggplot(data = area_nhood_zhvi, aes(x = date, y = zhvi)) +
  geom_point(data = filter(select(nhood_zhvi, -c(region_name)), year >= 2000), color = "gray80", alpha = 0.2, size = 0.75) +
  geom_point(aes(color = region_name), alpha = 0.9, size = 1.25) +
  facet_wrap(~region_name) +
  scale_color_brewer(palette = "Dark2") +
  hrbrthemes::theme_ipsum_pub() +
  labs(
    title = "Zillow Home Value Index (ZHVI) by neighborhood",
    color = "Neighborhood",
    x = "Date",
    y = "ZHVI (log)",
    caption = "Source: Zillow."
  ) +
  scale_y_log10(labels = scales::label_dollar())

