require(tidyverse)
library(tigris)
library(ggplot2)
library(patchwork)
library(mapview)
library(glue)


st <- states()

class(st)
plot(st$geometry)

nm_counties <- counties("NM")
plot(nm_counties$geometry)


la_tracts <- tracts("NM", "Los Alamos")
plot(la_tracts$geometry)


la_water <- area_water("NM", "Los Alamos")
plot(la_water$geometry)



dc_landmarks <- landmarks("DC", type = "point")
plot(dc_landmarks$geometry)

dc_roads <- primary_secondary_roads("DC")
plot(dc_roads$geometry)

dc_block_groups <- block_groups("DC")
plot(dc_block_groups$geometry)


ggplot(la_tracts) + 
  geom_sf() + 
  theme_void()


la_block_groups <- block_groups("NM", "Los Alamos")

gg1 <- ggplot(la_tracts) + 
  geom_sf() + 
  theme_void() + 
  labs(title = "Census tracts")

gg2 <- ggplot(la_block_groups) + 
  geom_sf() + 
  theme_void() + 
  labs(title = "Block groups")

gg1 + gg2


mapview(la_tracts)


mi_counties <- counties("MI")
mi_counties_cb <- counties("MI", cb = TRUE)

mi_tiger_gg <- ggplot(mi_counties) + 
  geom_sf() + 
  theme_void() + 
  labs(title = "TIGER/line")

mi_cb_gg <- ggplot(mi_counties_cb) + 
  geom_sf() + 
  theme_void() + 
  labs(title = "Cartographic boundary")

mi_tiger_gg + mi_cb_gg


options(tigris_use_cache = TRUE)
rappdirs::user_cache_dir("tigris")



yearly_plots <- map(seq(1990, 2020, 10), ~{
  year_tracts <- tracts("TX", "Tarrant", year = .x,
                        cb =TRUE)
  
  ggplot(year_tracts) + 
    geom_sf() + 
    theme_void() + 
    labs(title = glue("{.x}: {nrow(year_tracts)} tracts"))
})


(yearly_plots[[1]] + yearly_plots[[2]]) / 
  (yearly_plots[[3]] + yearly_plots[[4]])


us_bgs_2020 <- block_groups(cb = TRUE, year = 2020)
nrow(us_bgs_2020)


state_codes <- c(state.abb, "DC", "PR")

us_bgs_2018 <- map_dfr(
  state_codes,
  ~block_groups(
    state = .x,
    cb = TRUE,
    year = 2018
  )
)

nrow(us_bgs_2018)


library(sf)
fl_counties <- counties("FL", cb = TRUE)
st_crs(fl_counties)

install.packages("crsuggest")
library(crsuggest)

fl_crs <- suggest_crs(fl_counties)
fl_projected <- st_transform(fl_counties, crs = 3087)

head(fl_projected)

st_crs(fl_projected)


options(scipen = 999)

ggplot(fl_counties) + 
  geom_sf() + 
  coord_sf(crs = 3087)

ggplot(fl_counties) + 
  geom_sf() + 
  coord_sf(crs = 3087, datum = 3087)


us_states <- states(cb = TRUE, resolution = "20m")

ggplot(us_states) + 
  geom_sf() +
  theme_void()


ggplot(us_states) +
  geom_sf() +
  coord_sf(crs = "ESRI:102003") +
  theme_void()

us_states_shifted <- shift_geometry(us_states)

ggplot(us_states_shifted) + 
  geom_sf() + 
  theme_void()

us_states_outside <- shift_geometry(us_states,
                                    preserve_area = TRUE,
                                    position = "outside")

ggplot(us_states_outside) + 
  geom_sf() + 
  theme_void()


tx_places <- places("TX", cb = TRUE) %>%
  filter(NAME %in% c("Dallas", "Fort Worth", "Houston",
                     "Austin", "San Antonio", "EL Paso")) %>%
  st_transform(6580)

tx_outline <- states(cb = TRUE) %>%
  filter(NAME == "Texas") %>%
  st_transform(6580)

ggplot() +
  geom_sf(data = tx_outline) + 
  geom_sf(data = tx_places, fill = "red", color = NA) + 
  theme_void()

tx_centroids <- st_centroid(tx_places)
ggplot() + 
  geom_sf(data = tx_outline) + 
  geom_sf(data = tx_centroids, color = "red", size = 3) + 
  theme_void()


lee <- fl_projected %>%
  filter(NAME == "Lee")

mapview(lee)

lee

lee_singlepart <- st_cast(lee, "POLYGON")
lee_singlepart


sanibel <- lee_singlepart[2,]
mapview(sanibel)
