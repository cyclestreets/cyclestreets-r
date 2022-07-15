library(cyclestreets)
library(tidyverse)
library(stplanr)
library(sf)
l_desire = readRDS("~/OneDrive/projects-all/tiicycling/atumie/outputs/2022-05-31/rds/kildare/od_jittered_work.Rds")
l_desire_30 = l_desire[1:30, ]
sf::write_sf(l_desire_30, "ld30.geojson")
piggyback::pb_upload("ld30.geojson") # fails
piggyback::pb_releases()
system("gh release upload v0.5.3 ld30.geojson")
routes_route_30 = route(l = l_desire_30, route_fun = journey, plan = "quietest")
sf::write_sf(routes_route_30, "ld30-route-output.geojson")
summary(routes_route_30$route_number)
routes_batch_30 = batch(l_desire_30, strategies = "quietest", username = "robinlovelace")

file.copy("/tmp/Rtmp1E6NTc/test.csv", "ld30-batch-output.csv")
system("gh release upload v0.5.3 ld30-batch-output.csv")
file.edit("ld30.geojson") # check the geojson
# Coordinates of 2nd row of data:
# [ -6.985942, 52.9971794 ], [ -6.8965459, 52.8476297 ]

file.edit("ld30-batch-output.csv")
# Coordinates of 2nd row of data:
# --6.98191,52.99439 -6.98195,52.99431
file.edit("ld30-route-output.geojson")
# Coordinates of first departure point 2nd row of data:
# [ -6.98594, 52.99718 ], [ -6.98632, 52.99695 ]

nrow(routes_batch_30)
nrow(routes_route_30)
# fewer routes
summary(routes_batch_30$id)
identical(routes_batch_30$id, routes_route_30$route_number)
identical(routes_batch_30$geo_code1, routes_route_30$geo_code1)
identical(routes_batch_30$geometry, routes_route_30$geometry)
summary(routes_route_30$route_number %in% routes_batch_30$id)
routes_route_n = routes_route_30 |>
  group_by(route_number) |>
  summarise(nrow = n())
routes_batch_n = routes_batch_30 |>
  group_by(id) |>
  summarise(nrow = n())

l_desire$geometry[1] |> mapview::mapview()
l_desire$geometry[2] |> mapview::mapview()
routes_batch_n$geometry[1] |> mapview::mapview()
routes_route_n$geometry[1] |> mapview::mapview()
routes_batch_n$geometry[2] |> mapview::mapview()
routes_route_n$geometry[2] |> mapview::mapview()

plot(routes_route_n$route_number, routes_batch_n$id)
plot(routes_route_n$nrow, routes_batch_n$nrow)
routes_combined_n = cbind(routes_batch_n, nrowb = routes_route_n$nrow)
summary(routes_combined_n$nrowb %in% routes_combined_n$nrow)

