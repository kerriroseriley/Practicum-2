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
library(scales) # palette function that runs from black to red

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

# Filter strictly to New York State
ny_counties <- us_counties %>%
  filter(STUSPS == "NY")


# 1 : Use shapefile to draw a map of the counties of New York State (and only New York State - do not include counties from neighboring states).
# Plot the county boundaries
plot(st_geometry(ny_counties), col = "lightgrey", main = "Counties of New York State")


# 2: Using the same file, which is the largest county, by area, in New York? Which is the smallest?
largest_area <- ny_counties[]
smallest_area <- ny_counties[]


# 3: Use nyshighpoints.csv to plot the location of high points of every county in the state of New York over the base county map.
# add=TRUE draws over the plot, pch=16 makes it a solid dot
plot(st_geometry(points), pch=16, add=TRUE)

# 4: Use a color scale to mark the high points according to their height. Make sure to have at least four different colors included in your scale.

# Check outliers
points_sorted <- points[order(points$Elevation_Meters), ]
tail(points_sorted$Elevation_Meters, 10)
# 1629 is much higher but should not be splitted

plot(st_geometry(ny_counties), col = "lightgrey")
# Using Elevation_Meters 
plot(st_geometry(points), col = points$Elevation_Meters, pch = 16, add = TRUE)


# 5: What are the highest and lowest high points in the state?


# 6: Label the highest high point on a map


# 7: Use the tidycensus package to load county population in New York. Shade in each county according to its population. Use a light yellow to dark orange scale

census_api_key("6c0caa75ce44c147825255c4f7e660b86bdaf606")

# Load 2024 population by county, with geometry
counties <- get_decennial(
  geography = "county",
  variables = "P1_001N",   # total population
  year = 2020,
  geometry = TRUE
)

# Look at Data
counties

# Filter to NYS

# 8: Calculate population density for each county using population and land area. Shade in each county according to its population density. Use a light lavender to dark purple scale


# 9: What is the largest county, by population, in New York? What is the smallest? What is the most dense county in New York? What is the least dense?


# 10: Bonus: Use the Algara and Amlani’s county level data to create maps of two-party vote for each county in New York in the 2000, 2004, 2008, 2012, 2016, and 2020 elections. Show vote by partisan lean, where more Democratic voting counties are bluer, more Republican voting counties are redder, and evenly split counties are white. You may discount any third party votes.






