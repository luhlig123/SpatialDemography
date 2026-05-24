#4/8/2026, Leo Uhlig
#The purpose of this script is to explore tidycensus and NHGIS data
#Required Libraries: tidycensus, tidyverse, ggplot2
#-------------------------------------------------------------------

library(ggplot2)
library(tidycensus)
library(tidyverse)
library(scales)

TotalPopulation_2020 <- read.csv("~/Documents/GEOG490/LabPortfolio/Labs/Lab2/data/nhgis0001_csv/nhgis0001_ds258_2020_state.csv")

ggplot() + geom_col(
  data = TotalPopulation_2020,
  aes(x = reorder(STATE, U7H001),
      y = U7H001),
  fill = "darkred") + coord_flip() + labs(x = "State", y = "Total Population", title = "Total Population by State")

TotalPopulation_2020_TC <- get_decennial(geography = "state",
                                         variables = "P1_001N",
                                         year = 2020,
                                         geometry = FALSE)

view(TotalPopulation_2020_TC)

ggplot() + geom_col(
  data = TotalPopulation_2020_TC,
  aes(x = reorder(NAME, value),
      y = value),
  fill = "darkred") + coord_flip() + labs(x = "State", y = "Total Population", title = "Total Population by State")


median_age <- get_acs(
  geography = "county",
  variables = "B01002_001",
  year = 2020
)


arrange(median_age, estimate)
arrange(median_age, desc(estimate))

filter(median_age, estimate >= 50)

separate(
  median_age,
  NAME,
  into = c("county", "state"),
  sep = ", "
)

race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012"
)

az_race <- get_acs(
  geography = "county",
  state = "AZ",
  variables = race_vars,
  summary_var = "B03002_001",
  year = 2020
)

az_race_percent <- az_race %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, percent)

largest_group <- az_race_percent %>%
  group_by(NAME) %>%
  filter(percent == max(percent))

az_race_percent %>%
  group_by(variable) %>%
  summarize(median_pct = median(percent))

mn_hh_income <- get_acs(
  geography = "county",
  table = "B19001",
  state = "MN",
  year = 2016
)
mn_hh_income


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#LAB SUBMISSON

#Washington Data
wa_data <- get_acs(
  geography = "county",
  state = "WA",
  variables = "DP02_0068P",
  geometry = TRUE
)

median(wa_data$estimate)

#MOE visualization
wa_val <- get_acs(
  geography = "county",
  state = "WA",
  variables = c(value = "B25077_001"),
  year = 2020
) %>%
  mutate(NAME = str_remove(NAME, " County, Washington"))


ggplot(wa_val, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "lightblue") +
  theme_minimal(base_size = 12.5) +
  labs(
    title = "Median Home Value",
    subtitle = "Counties in Washington",
    x = "2016-2020 ACS estimate",
    y=""
  ) +
  scale_x_continuous(labels = label_dollar())

#population pyramid
washington <- get_estimates(
  geography = "state",
  state = "WA",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
)

washington_filtered <- filter(washington, str_detect(AGEGROUP, "^Age"),
                              SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

ggplot(washington_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col()
