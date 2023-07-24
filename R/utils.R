
txt2coords = function(txt) {
  coords_split = stringi::stri_split_regex(txt, pattern = " |,")[[1]]
  matrix(as.numeric(coords_split), ncol = 2, byrow = TRUE)
}
txt2coords2 = function(txt) {
  if(is.na(txt)){
    return(NULL)
  }
  #sf::st_linestring(txt2coords(txt))
  sfheaders::sfg_linestring(txt2coords(txt))
}

# f = system.file(package = "cyclestreets", "extdata/journey.json")
# obj = jsonlite::read_json(f, simplifyVector = TRUE)
# txt = obj$marker$`@attributes`$points[2]
# c1 = txt2coords(txt)
# c2 = txt2coords2(txt)
# c3 = txt2coords3(txt)
# waldo::compare(c1, c2)
# waldo::compare(c1, c3)
# # `old` is a double vector (-1.54408, -1.54399, -1.54336, -1.54331, -1.54329, ...)
# # `new` is an S3 object of class <XY/LINESTRING/sfg>, a double vector
# bench::mark(check = FALSE,
#   c1 = txt2coords(txt),
#   c2 = txt2coords2(txt),
#   c3 = txt2coords2(txt)
# )

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

