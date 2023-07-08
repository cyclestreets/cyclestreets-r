

# obj = jsonlite::read_json(f, simplifyVector = TRUE)

txt2coords = function(txt) {
  # helper function to document...
  coords_split = stringr::str_split(txt, pattern = " |,")[[1]]
  matrix(as.numeric(coords_split),
         ncol = 2,
         byrow = TRUE)
}
# txt2coords(obj$marker$`@attributes`$points[2])

# e = obj$marker$`@attributes`$elevations[1] # for whole journey
# e1 = obj$marker$`@attributes`$elevations[2] # for 1st segment
# txt = obj$marker$`@attributes`$elevations[2] # for 2nd segment

txt2elevations = function(txt) {
  # helper function to document...
  coords_split = stringr::str_split(txt, pattern = ",")[[1]]
  as.numeric(coords_split)
}

# x = 1:2
# route_rolling_average(x)
route_rolling_average = function(x, n = 3) {
  if(length(x) >= n) {
    as.numeric(stats::filter(x, rep(1 / n, n), sides = 2))
  } else {
    x
  }
}

txt2coords2 = function(txt) {
  if(is.na(txt)){
    return(NULL)
  }
  coords_split = stringr::str_split(txt, pattern = " |,")[[1]]
  coords_split = matrix(as.numeric(coords_split),
                        ncol = 2,
                        byrow = TRUE)
  sf::st_linestring(coords_split)
}


get_values = function(v, fun) {
  sapply(v, function(x) fun(as.numeric(x)))
}

extract_values = function(x) stringr::str_split(x, pattern = ",")
get_mean = function(v) get_values(v, fun = mean)
get_sum = function(v) get_values(v, fun = sum)
get_min = function(v) get_values(v, fun = min)
get_max = function(v) get_values(v, fun = max)

# Aim: add these columns
# [17] "gradient_segment"        "elevation_change"        "gradient_smooth"
# Tests:
# r1 = sf::read_sf("data-raw/r_1.geojson")
# add_columns(r1)

add_columns = function(r) {

  elevations_list = extract_values(r$elevations)

  elevation_min = get_min(elevations_list)
  elevation_max = get_max(elevations_list)
  distances_list = extract_values(r$distances)
  # # Should be this for clearer name:
  # r$segment_length = get_sum(distances_list)
  # But for compatibility with original journey() we'll go with this:
  r$distances = get_sum(distances_list)
  elevation_change = elevation_max - elevation_min
  # Order for compatibility with journey:
  r$gradient_segment = elevation_change / r$distances
  r$elevation_change = elevation_max - elevation_min
  r$gradient_smooth = cyclestreets::smooth_with_cutoffs(
    r$gradient_segment,
    r$elevation_change,
    r$distances,
    distance_cutoff = 50,
    gradient_cutoff = 0.1,
    n = 3,
  )
  r
}

