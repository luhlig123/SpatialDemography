#4/29/2026
#GEOG 490 Week 5 lab - Spatial Analysis and Modelling
#Author: Leo Uhlig

#The Purpose of this lab is to explore analysis using Rstudio
#-------------------------------------------------------------------------------

library(tigris)
library(mapview)
library(tidyverse)
library(tidycensus)
library(sf)
options(tigris_use_cache = TRUE)

#CRS: NAD83(2011) Kansas Reogional Coordinate System
#Zone 11 (Kansas City)
ks_mo_tracts <- map_dfr(c("KS", "MO"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(8528)

kc_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Kansas City")) %>%
  st_transform(8528)

ggplot() +
  geom_sf(data = ks_mo_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()


kc_tracts <- ks_mo_tracts[kc_metro, ]

ggplot() +
  geom_sf(data = kc_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()

#Spatial joins
gainesville_patients <- tibble(
  patient_id = 1:10,
  longitude = c(-82.308131, -82.311972, -82.361748, -82.374377,
                -82.38177, -82.259421, -82.367436, -82.404031,
                -82.43289, -82.461844),
  latitude = c(29.645933, 29.655195, 29.621759, 29.653576,
               29.677201, 29.674923, 29.71099, 29.711587,
               29.648227, 29.624037)
)

# CRS: NAD83(2011) / Flordia North
gainesville_sf <- gainesville_patients %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(6440)


mapview(
  gainesville_sf,
  col.regions = "red",
  legend = FALSE
)


alachua_insurance <- get_acs(
  geography = 'tract',
  variables = 'DP03_0096P',
  state = 'FL',
  county = 'Alachua',
  year = 2019,
  geometry = TRUE
) %>%
  select(GEOID, pct_insured = estimate,
         pct_insured_moe = moe) %>%
  st_transform(6440)

mapview(
  alachua_insurance,
  zcol = "pct_insured",
  layer.name = "% with health<br/>insurance"
) +

  mapview(
    gainesville_sf,
    col.regions = 'red',
    legend = FALSE
  )


patients_joined <- st_join(
  gainesville_sf,
  alachua_insurance
)



#CRS: NAD83(2011) / Texas Centric Albers Equal Area
tx_cbsa <- get_acs(
  geography = 'cbsa',
  variables = 'B01003_001',
  year = 2019,
  survey = 'acs1',
  geometry = TRUE
) %>%
  filter(str_detect(NAME, "TX")) %>%
  slice_max(estimate, n = 4) %>%
  st_transform(6579)

pct_hispanic <- get_acs(
  geography = 'tract',
  variables = 'DP05_0071P',
  state = "TX",
  year = 2019,
  geometry = TRUE
) %>%
  st_transform(6579)

hispanic_by_metro <- st_join(
  pct_hispanic,
  tx_cbsa,
  join = st_within,
  suffix = c("_tracts", "_metro"),
  left = FALSE
)


hispanic_by_metro %>%
  mutate(NAME_metro = str_replace(NAME_metro, ", TX Metro Area", "")) %>%
  ggplot() +
  geom_density(aes(x = estimate_tracts), color = "navy", fill = 'navy',
               alpha = 0.4)  +
  theme_minimal() +
  facet_wrap(~NAME_metro) +
  labs(title = "Distribution of Hispanic/Latino population by census tract",
       subtitle = "Largest metropolitain areas in texas",
       y = "Kernel density estimate",
       x = "Percent Hispanic/Latino in Census Tract")


median_by_metro <- hispanic_by_metro %>%
  group_by(NAME_metro) %>%
  summarize(median_hispanic = median(estimate_tracts, na.rm = TRUE))


