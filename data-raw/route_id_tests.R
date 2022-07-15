library(cyclestreets)
library(tidyverse)
library(stplanr)
library(sf)
# l_desire = readRDS("~/OneDrive/projects-all/tiicycling/atumie/outputs/2022-05-31/rds/kildare/od_jittered_work.Rds")
# l_desire_30 = l_desire[1:30, ]
# sf::write_sf(l_desire_30, "ld30.geojson", delete_dsn = TRUE)
# piggyback::pb_upload("ld30.geojson") # fails
# piggyback::pb_releases()
# system("gh release upload v0.5.3 ld30.geojson --clobber")
# sf::write_sf(routes_route_30, "ld30-route-output.geojson")
# l_desire_30 = sf::read_sf("ld30.geojson")
l_desire_30 = sf::read_sf("https://github.com/cyclestreets/cyclestreets-r/releases/download/v0.5.3/ld30.geojson")
nrow(l_desire_30)
routes_route_30 = route(l = l_desire_30, route_fun = journey, plan = "quietest")
summary(routes_route_30$route_number)
routes_batch_30 = batch(l_desire_30, strategies = "quietest", username = "robinlovelace")
setdiff(names(routes_batch_30), names(routes_route_30))

identical(routes_route_30$geometry[1], routes_batch_30$geometry[1]) # first 1 are identical
identical(routes_route_30$geometry, routes_batch_30$geometry) # all are identical
identical(routes_route_30$geo_code1, routes_batch_30$geo_code1) # all are identical
identical(routes_route_30$geo_code2, routes_batch_30$geo_code2) # false
identical(routes_route_30$foot, routes_batch_30$foot) # false
plot(1:nrow(routes_route_30), routes_route_30$cyclists)
plot(1:nrow(routes_route_30), routes_batch_30$cyclists)
head(which(routes_batch_30$id != routes_route_30$route_number))
# [1] 42 43 44 45 46 47
routes_route_30$route_number[41] # 2
routes_route_30$route_number[42] # 2
routes_route_30$route_number[43] # 2

routes_batch_30$id[41] # 2
routes_batch_30$id[42] # 3
routes_batch_30$id[43] # 3

head(which(routes_batch_30$foot != routes_route_30$foot))
# 63 is the first one out
routes_route_30[63, ]
routes_batch_30[63, ]

# Re-run batch command with browser() at breakpoint:
routes_batch_30 = batch(l_desire_30, strategies = "quietest", username = "robinlovelace")

# Source of the problem is basically this:
table(1:10)
table(as.character(1:10))

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

