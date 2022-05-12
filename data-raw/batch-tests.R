# Aim: test batch routing
library(stplanr)
library(cyclestreets)
library(tmap)
tmap_mode("view")

# input data
library(odrust)
od = readr::read_csv("https://github.com/dabreegster/odjitter/raw/main/data/od.csv")
zones = sf::read_sf("https://github.com/dabreegster/odjitter/raw/main/data/zones.geojson")
road_network = sf::read_sf("https://github.com/dabreegster/odjitter/raw/main/data/road_network.geojson")
set.seed(42)
od_jittered = odjitter::jitter(
  od,
  zones,
  subpoints = road_network,
  disaggregation_threshold = 50
)

od_geo = od_jittered[1:3, 0]
# od_geo$id = 1:3
coord_example = od::od_coordinates(od_geo)
readr::write_csv(as.data.frame(coord_example), "data-raw/coord_example.csv")
file.remove("data-raw/coord_example.geojson")
sf::write_sf(od_geo, "data-raw/coord_example.geojson")
coord_example_sf = sf::st_sf(coord_example, geometry = od_geo$geometry)
coord_example_cs = route(coord_example_sf, route_fun = journey)
sf::write_sf(coord_example_cs, "data-raw/batch_output_example_route_stplanr.geojson")

# Batch request -----------------------------------------------------------

?POST
batchroutes = function(base_url = "https://api.cyclestreets.net/v2/batchroutes.createjob") {
  batch_url = paste0(base_url, "?key=", Sys.getenv("CYCLESTREETS"))
  body = list(
    name = "Journey matrix for Cambridge",
    serverId = 21,
    geometry = '{"type": "FeatureCollection", "features": [
      {"type": "Feature", "id": 1, "properties": {}, "geometry": {"type": "Point", "coordinates": [0.14187, 52.20303]}},
      {"type": "Feature", "id": "a", "properties": {}, "geometry": {"type": "Point", "coordinates": [0.14711, 52.20061]}},
      {"type": "Feature", "id": 56, "properties": {}, "geometry": {"type": "Point", "coordinates": [0.11638, 52.20360]}}
    ]}',
    strategies = "fastest,quietest",
    bothDirections = 1,
    minDistance = 50,
    maxDistance = 5000,
    filename = "cambridge",
    includeJsonOutput = 1,
    emailOnCompletion = "webmaster@example.com",
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  httr::POST(url = batch_url, body = body)
  # return_url = ""
}

batchcontrol = function(base_url = "https://api.cyclestreets.net/v2/batchroutes.controljob") {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", Sys.getenv("CYCLESTREETS"))
  body = list(
    id = 196,
    action = "start",
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  httr::POST(url = batch_url, body = body)
}

# POST https://api.cyclestreets.net/v2/batchroutes.jobdata?key=...
# (
#   [id] => 69158
#   [username] => myusername
#   [password] => mypassword
# )

batchroutes = function(base_url = "https://api.cyclestreets.net/v2/batchroutes.jobdata", poll_interval = 60) {
  # POST https://api.cyclestreets.net/v2/batchroutes.controljob?key=...
  batch_url = paste0(base_url, "?key=", Sys.getenv("CYCLESTREETS"))
  body = list(
    id = 196,
    username = "robinlovelace",
    password = Sys.getenv("CYCLESTREETS_PW")
  )
  # TODO add polling
  message("Sending data, wait...")
  res = httr::POST(url = batch_url, body = body)
  res_json = httr::content(res, "parsed")
  if(res_json$ready) {
    message("Congrats, you data is ready 🎉")
    data_res = httr::GET(url = res_json$files$dataCsv)
  }
}

batch_read = function(file = "cambridge-data.csv.gz") {
  R.utils::gunzip("cambridge-data.csv.gz")
  res = readr::read_csv("cambridge-data.csv")
  jsonlite::parse_json(res$json[1])
}
download.file(url = "https://www.cyclestreets.net/journey/batch/195/cambridge-data.csv", "/tmp/test.csv")
file.edit("/tmp/test.csv")

# New batch inputs --------------------------------------------------------

routes_geojson = sf::read_sf("https://github.com/cyclestreets/cyclestreets-r/releases/download/v0.5.3/coord_example.geojson")
routes_geojson
qtm(routes_geojson)

# starting from outputs
piggyback::pb_list()
u1 = "https://github.com/cyclestreets/cyclestreets-r/releases/download/v0.5.3/batchtesting.-.with.Include.full.JSON.API.response.option.geojson"
u2 = "https://github.com/cyclestreets/cyclestreets-r/releases/download/v0.5.3/batchtesting.-.with.Include.route.string.column.geojson"
u3 = "https://github.com/cyclestreets/cyclestreets-r/releases/download/v0.5.3/batchtesting.-.with.neither.option.geojson"
route1 = sf::read_sf(u1)
route2 = sf::read_sf(u2)
route3 = sf::read_sf(u3)
route1
route2
route3
qtm(route1, col = names(route1)[1]) # it's a long route!
plot(route1$geometry[1], lwd = 5, col = "red")
plot(route1$geometry[2], add = TRUE)

# Let's take a look at segment level data
route1$distances
ldf = stplanr::line2df(route1)
lsf = od::odc_to_sf(ldf[, -1])
lsf = sf::st_sf(ldf, geometry = lsf$geometry)
readr::write_csv(ldf, "batch-test-input-rl.csv")
route4 = stplanr::route(l = lsf, silent = FALSE)
sf::st_write(route4, "batch-test-output-rl.geojson")
piggyback::pb_upload("batch-test-input-rl.csv")
piggyback::pb_upload("batch-test-output-rl.geojson")
j1 = jsonlite::read_json("journey1.json")
j2 = jsonlite::read_json("journey2.json")
j_combined = list(j1, j2)
jsonlite::write_json(j_combined, "j_combined.json", pretty = TRUE)

