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
