#GEOG 490 - Lab 3
#Author: Leo Uhlig

#The purpose of this lab is to explore mapping functions in R as detailed by
#Kyle Walker in Chapter 3 of Mapping Census Data with R
#-------------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)

#seattle metro income
SMA_income <- get_acs(
    geography = "tract",
    state = "WA",
    county = c("King"),
    variables = "B19013_001",
    year = 2020,
    survey = 'acs5',
    geometry = TRUE,
  ) %>%
    rename(Median_income = estimate)

#Seattle metro median income map
plot(SMA_income$geometry)
ggplot(data = SMA_income, aes(fill = Median_income)) +
  scale_fill_viridis_c() +
  geom_sf() +
  theme_void() +
  labs(
    title = "Median Income in King County, WA"
  )


#King county race and ethnicity
king_race <- get_decennial(
  geography = "tract",
  state = "WA",
  county = "King",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))


king_dots <- king_race %>%
  as_dot_density(
    value = "value",
    values_per_dot = 100,
    group = "variable"
  )

background_tracts <- filter(king_race, variable == "White")

tm_shape(background_tracts) +
  tm_polygons(col = 'white',
              border.col = "grey") +
  tm_shape(king_dots) +
  tm_dots(col = "variable",
          palette = "dark2",
          size = 0.1,
          title = "1 dot = 100 people") +
  tm_layout(legend.outside = TRUE,
            main.title = "King County Race and Ethnicity,\n2020 US Census")

king_black <- filter(king_race, variable == "Black")

#graduated symbols

v20 <- load_variables(year = 2020, dataset = "acs5")

SMA_poverty <- get_acs(
  geography = "tract",
  state = "WA",
  county = c("King"),
  variables = c("S1701_C03_003",
               "S1701_C03_008"),
  year = 2020,
  survey = 'acs5',
  geometry = TRUE,
) %>%
  rename(pop_below_poverty_line = estimate) %>%
  group_by(NAME) %>%
  summarise(total_sum = sum(pop_below_poverty_line, na.rm = TRUE))

tm_shape(SMA_poverty) + 
  tm_polygons() +
  tm_bubbles(size = "total_sum", alpha = 0.1,
             col = "navy",
             title.size = "King County Population Ages 0-34\nBelow the Poverty Line\n2020 US Census") +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom")
