

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

cyclestreets:::json2sf_cs()

split_elevations = stringr::str_split(r_3$elevations, pattern = ",")
r_3$elevation_mean = sapply(split_elevations, function(x) mean(as.numeric(x)))
r_3$elevation_min = sapply(split_elevations, function(x) min(as.numeric(x)))
r_3$elevation_max = sapply(split_elevations, function(x) max(as.numeric(x)))
r_3$gradient_segment = (r_3$elevation_max - r_3$elevation_min) / r_3$distances
d_variable$elevation_change = (vals_variable$elevation_max -
                                 vals_variable$elevation_min)

smooth_with_cutoffs(
  r$gradient_segment,
  r$elevation_change,
  r$distances,
  distance_cutoff,
  gradient_cutoff,
  n,
  warnNA = warnNA
)

route_rolling_average <- function(x, n = 3) {
  if(length(x) >= n) {
    as.numeric(stats::filter(x, rep(1 / n, n), sides = 2))
  } else {
    x
  }
}
