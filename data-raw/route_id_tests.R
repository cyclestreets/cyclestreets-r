library(cyclestreets)
library(tidyverse)
library(stplanr)
library(sf)
l_desire = readRDS("~/OneDrive/projects-all/tiicycling/atumie/outputs/2022-05-31/rds/kildare/od_jittered_work.Rds")
routes_route_100 = route(l = l_desire[1:100, ], route_fun = journey, plan = "quietest")
summary(routes_route_100$route_number)
routes_batch_100 = batch(l_desire[1:100, ], strategies = "quietest", username = "robinlovelace")
nrow(routes_batch_100)
nrow(routes_route_100)
# fewer routes
summary(routes_batch_100$id)
identical(routes_batch_100$id, routes_route_100$route_number)
identical(routes_batch_100$geo_code1, routes_route_100$geo_code1)
identical(routes_batch_100$geometry, routes_route_100$geometry)
summary(routes_route_100$route_number %in% routes_batch_100$id)
routes_route_n = routes_route_100 |>
  group_by(route_number) |>
  summarise(nrow = n())
routes_batch_n = routes_batch_100 |>
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

