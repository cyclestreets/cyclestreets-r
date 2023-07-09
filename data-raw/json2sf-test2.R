f = system.file(package = "cyclestreets", "extdata/journey.json")
obj = jsonlite::read_json(f, simplifyVector = TRUE)
rsf = json2sf_cs(obj, cols = c("distances"))
bench::mark(
  test = json2sf_cs(obj, cols = c("distances"))
)
# 90 itr/sec # Around 30 itr/sec for typical commute routes
f = function() {
  f = system.file(package = "cyclestreets", "extdata/journey.json")
  obj = jsonlite::read_json(f, simplifyVector = TRUE)
  json2sf_cs(obj, cols = c("distances"))
}
profvis::profvis(f())

devtools::load_all()
profvis::profvis(batch_read("test-data-7mb.csv"))

# Test single segment route:


origin = c(-0.214849749698027,51.5586119006974)
destination = c( -0.214888955159809,51.5564466729046)

a = journey(
  origin,
  destination
)
mapview::mapview(a)

# Context: https://github.com/cyclestreets/cyclestreets-r/issues/71
single_route = sf::read_sf("data-raw/cyclestreets90204532balanced.kml")
single_route$geometry
sf::st_coordinates(single_route$geometry)
library(tmap)
tmap_mode("view")
single_route$Name = c(1, 2)
qtm(single_route, lines.lwd = 5)
qtm(single_route[2, 1])
qtm(single_route[1, 1])
# Single route that doubles back...



from_point = c(-1.52460, 53.81057)
to_point =   c(-1.52548 ,53.80983)
single_route_cs = journey(from = from_point, to = to_point)
# save result from the API call to journey.json
qtm(single_route_cs)
res_json = journey(from_point, to_point, silent = FALSE, save_raw = TRUE)
f = "inst/extdata/single-journey.json"
jsonlite::write_json(res_json, f)
obj = jsonlite::read_json(f, simplifyVector = TRUE)
json2sf_cs(obj)
json2sf_cs(obj, smooth_gradient = TRUE, cols = c(
  "name",
  "distances",
  "time",
  "busynance",
  "elevations",
  "start_longitude",
  "start_latitude",
  "finish_longitude",
  "finish_latitude"
),
cols_extra = c(
  "crow_fly_distance",
  "event",
  "whence",
  "speed",
  "itinerary",
  "plan",
  "note",
  "length",
  "quietness",
  "west",
  "south",
  "east",
  "north",
  "leaving",
  "arriving",
  "grammesCO2saved",
  "calories",
  "edition",
  "gradient_segment",
  # "gradient_median",
  # "gradient_p75",
  # "gradient_max",
  "elevation_change",
  "provisionName"
))
