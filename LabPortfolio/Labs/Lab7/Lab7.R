#GEOG 490 Lab 7 - Segregation
#5/13/2026
#Author: Leo Uhlig

#The purpose of this lab is to replicate the multigroup segregation index
#technique detailed in Kyle Walker's "Analyzing US Census Data"
#-------------------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(segregation)
library(tigris)
library(sf)

#California data by race and ethnicity
ca_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ),
  state = "CA",
  geometry = TRUE,
  year = 2019
)

#urbanized areas by population with geometry,
#then filter for populations with 750,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME,
                                    fixed(", CA Urbanized Area (2010)")))

#inner spatial join between California tracts and urbanized area, returning
#tracts in the largest California urban areas with the urban column appended
ca_urban_data <- ca_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

ca_urban_data %>%
  filter(variable %in% c("white", "hispanic"),
         urban_name == "San Francisco--Oakland") %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )

ca_urban_data %>%
  filter(variable %in% c ("white", "hispanic")) %>%
  group_by(urban_name) %>%
  group_modify(~
                 dissimilarity(.x,
                               group = "variable",
                               unit = "GEOID",
                               weight = "estimate"
                               )
               ) %>%
  arrange(desc(est))


mutual_within(
  data = ca_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE
)

la_local_seg <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate",
    wide = TRUE
  )

la_tracts_seg <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_local_seg, by = "GEOID")

la_tracts_seg %>%
  ggplot(aes(fill = ls)) +
  geom_sf(color = NA) +
  coord_sf(crs = 26946) +
  scale_fill_viridis_c(option = "inferno") +
  theme_void() +
  labs(fill = "Local \nsegregation index")

la_entropy <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  group_by(GEOID) %>%
  group_modify(~data.frame(entropy = entropy(
    data = .x,
    group = "variable",
    weight = "estimate",
    base = 4)))

la_entropy_geo <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_entropy, by =  "GEOID")


library(mapboxapi)

la_city_hall <- mb_geocode("City Hall, Los Angeles CA")

minutes_to_downtown <- mb_matrix(la_entropy_geo, la_city_hall)


la_entropy_geo$minutes <- as.numeric(minutes_to_downtown)

ggplot(la_entropy_geo, aes(x = minutes_to_downtown, y = entropy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 80)) +
  labs(title = "Diversity Gradient, Los Angeles urbanized area",
       x = "Travel-time to downtown Los Angeles in minutes, census tracts",
       y = "entropy index")
