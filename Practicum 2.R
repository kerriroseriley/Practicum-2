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

# Calculate total area (land + water) in sq meters
ny_counties$total_area <- ny_counties$ALAND + ny_counties$AWATER

# Sort counties by total area from smallest to largest
ny_sorted <- ny_counties[order(ny_counties$total_area), ]

# Largest county
tail(ny_sorted[, c("NAME", "total_area")], 1)

# Smallest county
head(ny_sorted[, c("NAME", "total_area")], 1)


# 3: Use nyshighpoints.csv to plot the location of high points of every county in the state of New York over the base county map.
# add=TRUE draws over the plot, pch=16 makes it a solid dot
plot(st_geometry(points), pch=16, add=TRUE)

# 4: Use a color scale to mark the high points according to their height. Make sure to have at least four different colors included in your scale.

# Check outliers
points_sorted <- points[order(points$Elevation_Meters), ]
tail(points_sorted$Elevation_Meters, 10)
# 1629 is much higher but should not be splitted

# 5-color elevation palette (green -> yellow -> brown -> red -> white)
elev_pal <- gradient_n_pal(c("darkgreen", "yellow", "saddlebrown", "red", "white"))

# Rescale Elevation_Meters to [0, 1] and map to colors
vals  <- points$Elevation_Meters
rng   <- range(vals, na.rm = TRUE)
z     <- rescale(vals, to = c(0, 1), from = rng)
ptcol <- elev_pal(z)

# Plot the base map and colored high points
plot(st_geometry(ny_counties), col = "lightgrey", main = "NY County High Points by Elevation")
plot(st_geometry(points), pch = 21, bg = ptcol, col = "black", cex = 1.2, add = TRUE)

# Create elevation ranges and matching colors for the legend
breaks       <- round(seq(rng[1], rng[2], length.out = 6))
range_labels <- paste0(breaks[-6], "–", breaks[-1], " m")
mid_z        <- seq(0.1, 0.9, length.out = 5)

legend("topleft",
       legend = range_labels,
       pch    = 21,
       pt.bg  = elev_pal(mid_z),
       col    = "black",
       pt.cex = 1.2,
       title  = "Elevation",
       bty    = "o")

# 5: What are the highest and lowest high points in the state?
# Sort high points from lowest to highest elevation
points_sorted <- points[order(points$Elevation_Meters), ]

# Highest high point in NY
tail(points_sorted[, c("County", "High_Point_Name", "Elevation_Feet", "Elevation_Meters")], 1)

# Lowest high point in NY
head(points_sorted[, c("County", "High_Point_Name", "Elevation_Feet", "Elevation_Meters")], 1)


# 6: Label the highest high point on a map

# Separate the highest high point (Mount Marcy) from the rest of the points
highest_pt <- points[which.max(points$Elevation_Meters), ]
other_pts  <- points[-which.max(points$Elevation_Meters), ]

# Recompute colors for the regular points (or keep original ptcol without the max point)
ptcol_other <- ptcol[-which.max(points$Elevation_Meters)]

# Plot base map, regular points, and the highlighted highest point
plot(st_geometry(ny_counties), col = "lightgrey", main = "NY County High Points by Elevation")
plot(st_geometry(other_pts), pch = 21, bg = ptcol_other, col = "black", cex = 1.2, add = TRUE)
plot(st_geometry(highest_pt), pch = 24, bg = "red", col = "black", cex = 2.2, add = TRUE)

# Add Mount Marcy as its own labeled entry in the legend
legend("topleft",
       legend = c(range_labels, "Highest: Mount Marcy (1629 m)"),
       pch    = c(rep(21, 5), 24),
       pt.bg  = c(elev_pal(mid_z), "red"),
       col    = "black",
       pt.cex = c(rep(1.2, 5), 2.0),
       title  = "Elevation",
       bty    = "o")

# 7: Use the tidycensus package to load county population in New York. Shade in each county according to its population. Use a light yellow to dark orange scale
census_api_key("6c0caa75ce44c147825255c4f7e660b86bdaf606")

# Load 2024 population by county, with geometry
ny_pop <- get_decennial(
  geography = "county",
  variables = "P1_001N",   # total population
  state = "NY",
  year = 2020,
  geometry = FALSE
)

# Look at Data
pop

# Join population data onto your existing ny_counties shapefile by GEOID
ny_counties <- left_join(ny_counties, ny_pop[, c("GEOID", "value")], by = "GEOID")
names(ny_counties)[names(ny_counties) == "value"] <- "population"

# Create light yellow to dark orange color palette
pop_pal <- gradient_n_pal(c("lightyellow", "orange", "darkorange4"))

# Rescale population to [0, 1] and map to county fill colors
vals    <- ny_counties$population
rng     <- range(vals, na.rm = TRUE)
z       <- rescale(vals, to = c(0, 1), from = rng)
polycol <- pop_pal(z)

# Plot shaded NY counties
plot(st_geometry(ny_counties), col = polycol, border = "black",
     main = "New York County Population (2020)")

# Create population range brackets and matching legend
breaks       <- round(seq(rng[1], rng[2], length.out = 6))
range_labels <- paste0(format(breaks[-6], big.mark = ",", trim = TRUE), "–",
                       format(breaks[-1], big.mark = ",", trim = TRUE))
mid_z        <- seq(0.1, 0.9, length.out = 5)

legend("topleft",
       legend = range_labels,
       fill   = pop_pal(mid_z),
       border = "black",
       title  = "Population",
       bty    = "o")

# 8: Calculate population density for each county using population and land area. 
# Shade in each county according to its population density. Use a light lavender to dark purple scale.

# Calculate land area in square miles (1 sq mile = 2,589,988.11 sq meters) and population density
ny_counties$land_sq_mi  <- ny_counties$ALAND / 2589988.11
ny_counties$pop_density <- ny_counties$population / ny_counties$land_sq_mi

# Create light lavender to dark purple color palette
dens_pal <- gradient_n_pal(c("lavender", "mediumpurple", "purple4"))

# Apply a power transform (just like z ^ 0.7 in the Bosnia workshop) so upstate differences show up
vals    <- ny_counties$pop_density
rng     <- range(vals, na.rm = TRUE)
z       <- rescale(vals, to = c(0, 1), from = rng)
z_trans <- z ^ 0.3
polycol <- dens_pal(z_trans)

plot(st_geometry(ny_counties), col = polycol, border = "black",
     main = "New York County Population Density (per sq. mi.)")

# Back-transform the legend cutoffs so the numbers match the transformed colors
z_cuts       <- seq(0, 1, length.out = 6)
breaks       <- round(rng[1] + (z_cuts ^ (1 / 0.3)) * (rng[2] - rng[1]))
range_labels <- paste0(format(breaks[-6], big.mark = ",", trim = TRUE), "–",
                       format(breaks[-1], big.mark = ",", trim = TRUE))
mid_z        <- seq(0.1, 0.9, length.out = 5)

legend("topleft",
       legend = range_labels,
       fill   = dens_pal(mid_z),
       border = "black",
       title  = "Pop. Density (/sq mi)",
       bty    = "o")

# 9: What is the largest county, by population, in New York? What is the smallest? What is the most dense county in New York? What is the least dense?
# Sort by population
pop_sorted <- ny_counties[order(ny_counties$population), ]

# Largest county by population
tail(pop_sorted[, c("NAME", "population")], 1)

# Smallest county by population
head(pop_sorted[, c("NAME", "population")], 1)

# Sort by population density
dens_sorted <- ny_counties[order(ny_counties$pop_density), ]

# Most dense county
tail(dens_sorted[, c("NAME", "pop_density")], 1)

# Least dense county
head(dens_sorted[, c("NAME", "pop_density")], 1)

# 10: Bonus: Use the Algara and Amlani’s county level data to create maps of two-party vote for each county in New York in the 2000, 2004, 2008, 2012, 2016, and 2020 elections. Show vote by partisan lean, where more Democratic voting counties are bluer, more Republican voting counties are redder, and evenly split counties are white. You may discount any third party votes.

# Download the Harvard Dataverse file directly into my Data folder
url <- "https://dataverse.harvard.edu/api/access/datafile/5028532"
dest <- "~/Desktop/Current Classes/R/Practicum 2/Data/dataverse_shareable_presidential_county_returns_1868_2020.Rdata"

download.file(url, destfile = dest, mode = "wb")

# Load it into your R Environment
load(dest)

# 10: Bonus: Two-party presidential vote share in NY (2000–2020)

# Pad FIPS codes to 5 digits so NY counties start with "36"
# (Swap 'fips', 'election_year', 'democratic_raw_votes', 'republican_raw_votes' if names() differs)
pres_elections_release$GEOID <- sprintf("%05d", as.numeric(pres_elections_release$fips))

# Filter to NY ("36") and the 6 presidential election years
years   <- c(2000, 2004, 2008, 2012, 2016, 2020)
ny_elec <- pres_elections_release[substr(pres_elections_release$GEOID, 1, 2) == "36" &
                                    pres_elections_release$election_year %in% years, ]

# Calculate Democratic share of the two-party vote (0 = 100% R, 0.5 = even, 1 = 100% D)
ny_elec$dem_two_party <- ny_elec$democratic_raw_votes /
  (ny_elec$democratic_raw_votes + ny_elec$republican_raw_votes)

# Create diverging Red -> White -> Blue palette
partisan_pal <- gradient_n_pal(c("red3", "white", "blue3"))

# Plot all 6 elections in a 2x3 grid
par(mfrow = c(2, 3), mar = c(1, 1, 3, 1))

for (yr in years) {
  yr_data <- ny_elec[ny_elec$election_year == yr, c("GEOID", "dem_two_party")]
  map_df  <- left_join(ny_counties, yr_data, by = "GEOID")
  
  # Center white at 0.5 (50/50 split)
  z       <- rescale_mid(map_df$dem_two_party, to = c(0, 1), from = c(0, 1), mid = 0.5)
  polycol <- partisan_pal(z)
  
  plot(st_geometry(map_df), col = polycol, border = "black",
       main = paste(yr, "Presidential Election"))
  
  legend("topleft",
         legend = c("100% Dem", "75% Dem", "50% / 50%", "75% Rep", "100% Rep"),
         fill   = partisan_pal(c(1, 0.75, 0.5, 0.25, 0)),
         border = "black",
         cex    = 0.75,
         bty    = "o")
}

# Reset plotting layout back to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)






