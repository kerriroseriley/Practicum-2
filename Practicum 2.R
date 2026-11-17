#Title: Practicum 2
#Author: Kerri Rose Riley
#Purpose: Map New York State county boundaries, elevation high points, population density, and 2000–2020 presidential election results
#Requires: 2024 US County Shapefile (cb_2024_us_county_500k.shp), NYS High Points Dataset (nyshighpoints.csv), US Census API, and Algara & Amlani County Election Dataset
#Output: Figures and Quarto Document typeset as a .pdf


# Set up

# load in packages
library(sf) # reads shapefile - simple features
library(tidyverse) # Includes ggplots2, dplyr, readr
library(tidycensus) # Pulls NY county pop data from US census API


# crs = 4326 is WGS 84, the ordinary latitude and longitude your phone’s GPS uses.


# POINTS
# Read in high points data set from my Data folder
points <- read.csv("~/Desktop/Current Classes/R/Practicum 2/Data/nyshighpoints.csv")
summary(points)

points <- st_as_sf(points, coords = c("Longitude", "Latitude"), crs = 4326)
points

plot(points)
plot(st_geometry(points))

# POLYGONS
# Read in the shapefile from my Data folder
us_counties <- st_read("~/Desktop/Current Classes/R/Practicum 2/Data/cb_2024_us_county_500k/cb_2024_us_county_500k.shp", crs = 4326)
us_counties

plot(us_counties)
plot(st_geometry(us_counties), col="lightgrey")

plot(st_geometry(points), pch=16, add=TRUE)



# Filter strictly to New York State
ny_counties <- counties_us %>%
  filter(STUSPS == "NY")


# 1 : Use shapefile to draw a map of the counties of New York State (and only New York State - do not include counties from neighboring states).




# 2: Using the same file, which is the largest county, by area, in New York? Which is the smallest?


# 3: Use nyshighpoints.csv to plot the location of high points of every county in the state of New York over the base county map.


# 4: Use a color scale to mark the high points according to their height. Make sure to have at least four different colors included in your scale.


# 5: What are the highest and lowest high points in the state?


# 6: Label the highest high point on a map


# 7: Use the tidycensus package to load county population in New York. Shade in each county according to its population. Use a light yellow to dark orange scale



# 8: Calculate population density for each county using population and land area. Shade in each county according to its population density. Use a light lavender to dark purple scale


# 9: What is the largest county, by population, in New York? What is the smallest? What is the most dense county in New York? What is the least dense?


# 10: Bonus: Use the Algara and Amlani’s county level data to create maps of two-party vote for each county in New York in the 2000, 2004, 2008, 2012, 2016, and 2020 elections. Show vote by partisan lean, where more Democratic voting counties are bluer, more Republican voting counties are redder, and evenly split counties are white. You may discount any third party votes.






