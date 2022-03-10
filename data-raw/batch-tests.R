# Aim: test batch routing
library(stplanr)
library(cyclestreets)

# input data
?odrust::odr_jitter
library(odrust)
od = readr::read_csv("https://github.com/dabreegster/odjitter/raw/main/data/od.csv")
zones = sf::read_sf("https://github.com/dabreegster/odjitter/raw/main/data/zones.geojson")
road_network = sf::read_sf("https://github.com/dabreegster/odjitter/raw/main/data/road_network.geojson")
set.seed(42)
od_jittered = odr_jitter(
  od,
  zones,
  subpoints = road_network,
  disaggregation_threshold = 50
)

od_geo = od_jittered[1:3, 0]
coord_example = od::od_coordinates(od_geo)
readr::write_csv(as.data.frame(coord_example), "data-raw/coord_example.csv")
sf::write_sf(od_geo, "data-raw/coord_example.geojson")
coord_example_sf = sf::st_sf(coord_example, geometry = od_geo$geometry)
coord_example_cs = route(coord_example_sf, route_fun = journey)
sf::write_sf(coord_example_cs, "data-raw/batch_output_example_route_stplanr.geojson")
