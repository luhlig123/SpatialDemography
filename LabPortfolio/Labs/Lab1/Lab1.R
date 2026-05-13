library("tidycensus")
census_api_key("5bdf6ebfa25b4a647b5f0e11d4c95c7ae764bb74", install = TRUE)

total_population_10 <- get_decennial(
  geography = "state",
  variables = "P001001",
  year = 2010
)

total_population_10
