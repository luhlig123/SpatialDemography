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


#LAUDET AND IMAI CHAPTER 4
#country gdp data
co <- read.csv("countries.csv")

#gdp plot
plot(x = co$prior_gdp, y = co$gdp)

#gdp correlation
cor(co$gdp, co$prior_gdp)

#fit linear model
fit <- lm(co$gdp ~ co$prior_gdp)
abline(fit)

#log gdp values
co$log_gdp <- log(co$gdp)
co$log_prior_gdp <- log(co$prior_gdp)

#gdp histograms
hist(co$gdp)
hist(co$log_gdp)
hist(co$prior_gdp)
hist(co$log_prior_gdp)


#log transformed gdp plot
plot(x = co$log_prior_gdp, y = co$log_gdp)

#log correlation
cor(co$log_prior_gdp, co$log_gdp)

#fit log model
log_fit <- lm(log_gdp ~ log_prior_gdp, data = co)
abline(log_fit)


#gdp and light change variables
co$gdp_change <- ((co$gdp - co$prior_gdp) / co$prior_gdp) * 100
co$light_change <- ((co$light - co$prior_light) / co$prior_light) * 100


#light and gdp change visualizations
hist(co$gdp_change)
hist(co$light_change)

plot(x = co$light_change, y = co$gdp_change)

#light and gdp change correlation
cor(co$light_change, co$gdp_change)

#light/gdp change model
light_gdp_fit <- lm(gdp_change ~ light_change, data = co)
abline(light_gdp_fit)


#assessing fit with r^2 values
cor(co$gdp, co$prior_gdp)^2
cor(co$log_gdp, co$log_prior_gdp)^2
cor(co$gdp_change, co$light_change)^2

#---------------------------------------
#WALKER CHAPTER 8.2

#dallas fortworth
dfw_counties <- c("Collin County", "Dallas", "Denton",
                  "Ellis", "Hunt", "Kaufman", "Rockwall",
                  "Johnson", "Parker", "Tarrant", "Wise")

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

dfw_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "TX",
  county = dfw_counties,
  geometry = TRUE,
  output = 'wide',
  year = 2020
) %>%
  select(-NAME) %>%
  st_transform(32138) #NAD83/TEXAS NORTH CENTRAL

#Median Home Value Map
mhv_map <- ggplot(dfw_data, aes(fill = median_valueE)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median home value")

#Median Home Value histogram
mhv_histogram <- ggplot(dfw_data, aes(x = median_valueE)) +
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) +
  theme_minimal() +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = "Median home value")

mhv_map + mhv_histogram

#log maps
mhv_map_log <- ggplot(dfw_data, aes(fill = log(median_valueE))) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median home value\nvalue (log)")

mhv_histogram_log <- ggplot(dfw_data, aes(x = log(median_valueE))) +
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) +
  theme_minimal() +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = "Median home value (Log)")


mhv_map_log + mhv_histogram_log


#Feature engineering
dfw_data_for_model <- dfw_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.),"1/km2")),
         median_structure_age = 2018 - median_year_builtE) %>%
  select(!ends_with("M")) %>%
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()


#model
formula <- paste0("log(median_value) ~ median_rooms + median_income + ",
                  "pct_college + pct_foreign_born + pct_white + ",
                  "median_age + median_structure_age + ",
                  "percent_ooh + pop_density + total_population")

model1 <- lm(formula = formula, data = dfw_data_for_model)

summary(model1)

#correlation matrix
dfw_estimates <- dfw_data_for_model %>%
  select(-GEOID, -median_value, -median_year_built) %>%
  st_drop_geometry()

correlations <- correlate(dfw_estimates, method = "pearson")
network_plot(correlations)

vif(model1)

#Model2
formula2 <- paste0("log(median_value) ~ median_rooms + pct_college + ",
                   "pct_foreign_born + pct_white + median_age + ",
                   "median_structure_age + percent_ooh + pop_density + ",
                   "total_population")
model2 <- lm(formula = formula2, data = dfw_data_for_model)
summary(model2)

vif(model2)


#PCA
pca <- prcomp(
  formula = ~.,
  data = dfw_estimates,
  scale. = TRUE,
  center = TRUE
)
summary(pca)


pca_tibble <- pca$rotation %>%
  as_tibble(rownames = "predictor")

pca_tibble %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) +
  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL, x = "Value") +
  theme_minimal()

components <- predict(pca, dfw_estimates)

dfw_pca <- dfw_data_for_model %>%
  select(GEOID, median_value) %>%
  cbind(components)

ggplot(dfw_pca, aes(fill = PC1)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c()

pca_formula <- paste0("log(median_value) ~ ",
                      paste0('PC', 1:6, collapse = " + "))

pca_model <- lm(formula = pca_formula, data = dfw_pca)

summary(pca_model)
#-------------------------------------------------------------------------------
#LAB 9
#8.3 -> Spatial Regression

#calculate residuals
dfw_data_for_model$residuals <- residuals(model2)

#plot residuals
ggplot(dfw_data_for_model, aes(x = residuals)) +
  geom_histogram(bins = 100, alpha = 0.5, color = "navy", fill = "navy") +
  theme_minimal()

#moran test
wts <- dfw_data_for_model %>%
  poly2nb() %>%
  nb2listw()

moran.test(dfw_data_for_model$residuals, wts)

dfw_data_for_model$lagged_residuals <- lag.listw(wts, dfw_data_for_model$residuals)

ggplot(dfw_data_for_model, aes(x = residuals, y = lagged_residuals)) +
  theme_minimal() +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red")

#lagged residuals model
lag_model <- lagsarlm(
  formula = formula2,
  data = dfw_data_for_model,
  listw = wts
)

summary(lag_model, Nagelkerke = TRUE)

#error model
error_model <- errorsarlm(
  formula = formula2,
  data = dfw_data_for_model,
  listw = wts
)

summary(error_model, Nagelkerke = TRUE)

moran.test(lag_model$residuals, wts)
moran.test(error_model$residuals, wts)


lm.LMtests(
  model2,
  wts,
  test = c("LMerr", "LMlag", "RLMerr", "RLMlag")
)

#8.4 - GEOGRAPHICALLYY WEIGHTED REGRESSION

dfw_data_sp <- dfw_data_for_model %>%
  as_Spatial()

#kernel bandwidth
bw <- bw.gwr(
  formula = formula2,
  data = dfw_data_sp,
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
  data = dfw_data_sp,
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)

names(gw_model)

gw_model_results <- gw_model$SDF %>%
  st_as_sf()

names(gw_model_results)


ggplot(gw_model_results, aes(fill = Local_R2)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void()

ggplot(gw_model_results, aes(fill = percent_ooh)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Local ? for \npercent_ooh")

ggplot(gw_model_results, aes(fill = pop_density)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Local ? for \npopulation density")

#8.5 - CLASSIFICATION AND CLUSTERING

set.seed(1983)

dfw_kmeans <- dfw_pca %>%
  st_drop_geometry() %>%
  select(PC1:PC8) %>%
  kmeans(centers = 6)

table(dfw_kmeans$cluster)

dfw_clusters <- dfw_pca %>%
  mutate(cluster = as.character(dfw_kmeans$cluster))

ggplot(dfw_clusters, aes(fill = cluster)) +
  geom_sf(size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_void() +
  labs(fill = "Cluster")

#plot clusters
cluster_plot <- ggplot(dfw_clusters, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

ggplotly(cluster_plot) %>%
  layout(legend = list(orientation = "h", y = -0.15,
                       x = 0.2, title = "Cluster"))

#regionalization

input_vars <- dfw_pca %>%
  select(PC1:PC8) %>%
  st_drop_geometry() %>%
  as.data.frame()

skater_nbrs <- poly2nb(dfw_pca, queen = TRUE)
costs <- nbcosts(skater_nbrs, input_vars)
skater_weights <- nb2listw(skater_nbrs, costs, style = "B")

mst <- mstree(skater_weights)

regions <- skater(
  mst[,1:2],
  input_vars,
  ncuts = 7,
  crit = 10
)

dfw_clusters$region <- as.character(regions$group)

ggplot(dfw_clusters, aes(fill = region)) +
  geom_sf(size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_void()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
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
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Seattle Metro Area Median Household Income")



#Median Home Value histogram
mhv_histogram_sm <- ggplot(sm_data, aes(x = log(median_incomeE))) +
  geom_histogram(alpha = 0.5, fill = "darkblue", color = "gray", bins = 100) +
  theme_minimal() +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = "Seattle Metro Area log Median Household Income")

mhv_map_sm + mhv_histogram_sm

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


pca_tibble_sm <- pca$rotation %>%
  as_tibble(rownames = "predictor")

pca_tibble_sm %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) +
  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL, x = "Value") +
  theme_minimal()

components_sm <- predict(pca_sm, sm_estimates)

sm_pca <- sm_data_for_model %>%
  select(GEOID, median_value) %>%
  cbind(components_sm)

ggplot(sm_pca, aes(fill = PC1)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c()

pca_formula_sm <- paste0("log(median_value) ~ ",
                      paste0('PC', 1:6, collapse = " + "))

pca_model_sm <- lm(formula = pca_formula_sm, data = sm_pca)

summary(pca_model_sm)

#-------------------------------------------------------------------------------
#LAB 9 SUBMISSION

#8.4 - GEOGRAPHICALLY WEIGHTED REGRESSION

sm_data_sp <- sm_data_for_model %>%
  as_Spatial()

#kernel bandwidth
bw <- bw.gwr(
  formula = formula2,
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
  scale_fill_viridis_c() +
  theme_void()

sm_percent_ooh <- ggplot(gw_model_results, aes(fill = percent_ooh)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Local ? for \npercent_ooh")

sm_population_density <- ggplot(gw_model_results, aes(fill = pop_density)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Seattle Metro Area \npopulation density")

sm_population_density + sm_percent_ooh + sm_local_r2


#8.5 - CLASSIFICATION AND CLUSTERING

set.seed(1983)

sm_kmeans <- sm_pca %>%
  st_drop_geometry() %>%
  select(PC1:PC8) %>%
  kmeans(centers = 6)

table(sm_kmeans$cluster)

sm_clusters <- sm_pca %>%
  mutate(cluster = as.character(sm_kmeans$cluster))

ggplot(sm_clusters, aes(fill = cluster)) +
  geom_sf(size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_void() +
  labs(fill = "Cluster")

#plot clusters
cluster_plot <- ggplot(sm_clusters, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

ggplotly(cluster_plot) %>%
  layout(legend = list(orientation = "h", y = -0.15,
                       x = 0.2, title = "Cluster"))

#regionalization

input_vars <- sm_pca %>%
  select(PC1:PC8) %>%
  st_drop_geometry() %>%
  as.data.frame()



skater_nbrs <- poly2nb(sm_pca, queen = TRUE)
costs <- nbcosts(skater_nbrs, input_vars)
skater_weights <- nb2listw(skater_nbrs, costs, style = "B")

mst <- mstree(skater_weights)

regions <- skater(
  mst[,1:2],
  input_vars,
  ncuts = 7,
  crit = 10
)

sm_clusters$region <- as.character(regions$group)

ggplot(sm_clusters, aes(fill = region)) +
  geom_sf(size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  theme_void()
