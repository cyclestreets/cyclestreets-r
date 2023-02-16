

# Aim: get new columns in 'r_2' object
library(tidyverse)


devtools::load_all()
r_1 = sf::read_sf("data-raw/r_1.geojson")
r_2 = sf::read_sf("data-raw/r_2.geojson")
od = sf::read_sf("data-raw/od-test.geojson")

# Look at file:
file.edit("R/journey.R")
# [17] "gradient_segment"        "elevation_change"        "gradient_smooth"

r_3 = r_2

split_elevations = stringr::str_split(r_3$elevations, pattern = ",")

r_3$elevation_mean = sapply(split_elevations, function(x) mean(as.numeric(x)))

# Generalise the function for shorter lines:
get_values = function(v, fun) {
  sapply(v, function(x) fun(as.numeric(x)))
}
extract_values = function(x) stringr::str_split(x, pattern = ",")
get_mean = function(v) get_values(v, fun = mean)
get_sum = function(v) get_values(v, fun = sum)
get_min = function(v) get_values(v, fun = min)
get_max = function(v) get_values(v, fun = max)
elevations_list = extract_values(r_3$elevations)

r_3$elevations
r_3$elevation_min = get_min(elevations_list)
r_3$elevation_max = get_max(elevations_list)
distances_list = extract_values(r_3$distances)
r_3$segment_length = get_sum(distances_list)
r_3$elevation_change = r_3$elevation_max - r_3$elevation_min
r_3$gradient_segment = r_3$elevation_change / r_3$segment_length
r_3$gradient_smooth = cyclestreets::smooth_with_cutoffs(
  r_3$gradient_segment,
  r_3$elevation_change,
  r_3$segment_length,
  distance_cutoff = 50,
  gradient_cutoff = 0.1,
  n = 3,
)
identical(r_3$gradient_segment, r_3$gradient_smooth)

r_3 = r_3[names(r_1)]

waldo::compare(r_1, r_3)
