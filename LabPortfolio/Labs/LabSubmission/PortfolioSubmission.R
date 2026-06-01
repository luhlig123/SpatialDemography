#4/8/2026, Leo Uhlig
#The purpose of this script is to explore tidycensus and NHGIS data
#Required Libraries: tidycensus, tidyverse, ggplot2
#-------------------------------------------------------------------
#LAB 2 SUBMISSION
library(ggplot2)
library(tidycensus)
library(tidyverse)
library(scales)

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


WA_median_home_value <- ggplot(wa_val, aes(x = estimate, y = reorder(NAME, estimate))) +
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
WA_median_home_value
ggsave("WA_median_home_value.png", WA_median_home_value)

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

WA_pop_pyramid <- ggplot(washington_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col() +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)),
    breaks = seq(from = -350000, to = 350000, by = 100000)
  ) +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(
    title = "Washington Population by Age and Sex",
    y = "2020 US Census Estimates"
  )
WA_pop_pyramid
ggsave("WA_pop_pyramid.png", WA_pop_pyramid)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#GEOG 490 - Lab 3
#Author: Leo Uhlig

#The purpose of this lab is to explore mapping functions in R as detailed by
#Kyle Walker in Chapter 3 of Mapping Census Data with R
#-------------------------------------------------------------------------------
#LAB 3 SUBMISSION
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(Cairo)

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
king_income <-ggplot(data = SMA_income, aes(fill = Median_income)) +
  scale_fill_viridis_c() +
  geom_sf() +
  theme_void() +
  labs(
    title = "Median Income in King County, WA"
  )
king_income
ggsave("king_income.png", king_income)

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

library(Cairo)

king_dot_density <-tm_shape(background_tracts) +
  tm_polygons(col = 'white',
              border.col = "grey") +
  tm_shape(king_dots) +
  tm_dots(col = "variable",
          palette = "dark2",
          size = 0.1,
          title = "1 dot = 100 people") +
  tm_layout(legend.outside = TRUE,
            main.title = "King County Race and Ethnicity,\n2020 US Census")
king_dot_density
tmap_save(tm = king_dot_density, filename = "king_dot_density.pdf")

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

king_county_poverty <- tm_shape(SMA_poverty) + 
  tm_polygons() +
  tm_bubbles(size = "total_sum", alpha = 0.1,
             col = "red",
             title.size = "King County Population Ages 0-34\nBelow the Poverty Line\n2020 US Census") +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom")
king_county_poverty
tmap_save(tm = king_county_poverty, filename = "king_county_poverty.pdf")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#GEOG 490 - Spatial Demography Week 8 Lab
#Intro To Regression
#5/20/2026
#Author: Leo Uhlig

#The purpose of this lab is to introduce the concepts of linear regression
#as detailed in Data Analysis for Social Science by Elena Laudet and Kosuke Imai,
#as well as Census Data in R by Kyle Walker
#-------------------------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(sf)
library(patchwork)
library(units)
library(corrr)
library(car)
library(spdep)
library(spatialreg)
library(GWmodel)
library(plotly)

#LAB 8 SUBMISSION
#WALKER 8.2.2 recreation

#Seattle metro counties
smc <- c("King", "Snohomish", "Pierce")

variables_to_get <- c(
  median_value = "B25077_001",
  median_rooms = "B25018_001",
  median_income = "DP03_0062",
  total_population = "B01003_001",
  median_age = "B01002_001",
  pct_college = "DP02_0068P",
  pct_foreign_born = "DP02_0094P",
  pct_white = "DP05_0077P",
  median_year_built = "B25037_001",
  percent_ooh = "DP04_0046P"
)

sm_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "WA",
  county = smc,
  geometry = TRUE,
  output = 'wide',
  year = 2020
) %>%
  select(-NAME) #%>%
#st_transform(32138) #NAD83/TEXAS NORTH CENTRAL

#Median Home Value Map
mhv_map_sm <- ggplot(sm_data, aes(fill = log(median_incomeE))) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma",
                       labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Seattle Metro Area\nLog Median Household Income")



#Median Home Value histogram
mhv_histogram_sm <- ggplot(sm_data, aes(x = log(median_incomeE))) +
  geom_histogram(alpha = 0.5, fill = "purple", color = "darkgray", bins = 100) +
  theme_minimal() +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = "Seattle Metro Area\nLog Median Household Income")

mhv_map_sm + mhv_histogram_sm
ggsave("SMA_MEDIAN_INCOME.png", mhv_map_sm + mhv_histogram_sm)


#-------------------------------------------------------------------------------
#WALKER 8.2.5

#Feature engineering
sm_data_for_model <- sm_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.),"1/km2")),
         median_structure_age = 2018 - median_year_builtE) %>%
  select(!ends_with("M")) %>%
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()


#model
formula_sm <- paste0("log(median_value) ~ median_rooms + median_income + ",
                     "pct_college + pct_foreign_born + pct_white + ",
                     "median_age + median_structure_age + ",
                     "percent_ooh + pop_density + total_population")

model1_sm <- lm(formula = formula_sm, data = sm_data_for_model)

summary(model1_sm)

#correlation matrix
sm_estimates <- sm_data_for_model %>%
  select(-GEOID, -median_value, -median_year_built) %>%
  st_drop_geometry()

correlations_sm <- correlate(sm_estimates, method = "pearson")
network_plot(correlations_sm)

vif(model1_sm)

#Model2
formula2_sm <- paste0("log(median_value) ~ median_rooms + pct_college + ",
                      "pct_foreign_born + pct_white + median_age + ",
                      "median_structure_age + percent_ooh + pop_density + ",
                      "total_population")

model2_sm <- lm(formula = formula2_sm, data = sm_data_for_model)
summary(model2_sm)

vif(model2_sm)


#PCA
pca_sm <- prcomp(
  formula = ~.,
  data = sm_estimates,
  scale. = TRUE,
  center = TRUE
)
summary(pca_sm)


pca_tibble_sm <- pca_sm$rotation %>%
  as_tibble(rownames = "predictor")

PCs <- pca_tibble_sm %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) +
  geom_col(fill = "darkblue", color = "darkblue", alpha = 0.5) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL, x = "Value") +
  theme_minimal()
PCs

components_sm <- predict(pca_sm, sm_estimates)

sm_pca <- sm_data_for_model %>%
  select(GEOID, median_value) %>%
  cbind(components_sm)

PC1 <- ggplot(sm_pca, aes(fill = PC1)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c(option = "magma")

pca_formula_sm <- paste0("log(median_value) ~ ",
                         paste0('PC', 1:6, collapse = " + "))

pca_model_sm <- lm(formula = pca_formula_sm, data = sm_pca)

summary(pca_model_sm)

PCs + PC1
ggsave("SMA_PCA.png", PCs + PC1)

#-------------------------------------------------------------------------------
#LAB 9 SUBMISSION

#8.4 - GEOGRAPHICALLY WEIGHTED REGRESSION

sm_data_sp <- sm_data_for_model %>%
  as_Spatial()

#kernel bandwidth
bw <- bw.gwr(
  formula = formula2_sm,
  data = sm_data_sp,
  kernel = "bisquare",
  adaptive = TRUE
)

#fitting and evaluating GWR
formula2 <- paste0("log(median_value) ~ median_rooms + pct_college + ",
                   "pct_foreign_born + pct_white + median_age + ",
                   "median_structure_age + percent_ooh + pop_density + ",
                   "total_population")

gw_model <- gwr.basic(
  formula = formula2,
  data = sm_data_sp,
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)

names(gw_model)

gw_model_results <- gw_model$SDF %>%
  st_as_sf()

names(gw_model_results)


sm_local_r2 <- ggplot(gw_model_results, aes(fill = Local_R2)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma") +
  theme_void()

sm_percent_ooh <- ggplot(gw_model_results, aes(fill = percent_ooh)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma") +
  theme_void() +
  labs(fill = "Seattle Metro Area \npercent_ooh")

sm_population_density <- ggplot(gw_model_results, aes(fill = pop_density)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma") +
  theme_void() +
  labs(fill = "Seattle Metro Area \npopulation density")

sm_population_density + sm_percent_ooh + sm_local_r2

ggsave("sma_weighted.png", sm_population_density + sm_percent_ooh + sm_local_r2)


#8.5 - CLASSIFICATION AND CLUSTERING

set.seed(1983)

sm_kmeans <- sm_pca %>%
  st_drop_geometry() %>%
  select(PC1:PC8) %>%
  kmeans(centers = 6)

table(sm_kmeans$cluster)

sm_clusters <- sm_pca %>%
  mutate(cluster = as.character(sm_kmeans$cluster))

sma_cluster_plot <- ggplot(sm_clusters, aes(fill = cluster)) +
  geom_sf(size = 0.1) +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  labs(fill = "Cluster",
       title = "Seattle Metro Area PCA Clusters")
sma_cluster_plot
ggsave("sma_cluster_plot.png", sma_cluster_plot)

#plot clusters
cluster_plot <- ggplot(sm_clusters, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

ggplotly(cluster_plot) %>%
  layout(legend = list(orientation = "h", y = -0.15,
                       x = 0.2, title = "Cluster"))
