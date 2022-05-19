remotes::install_github("cyclestreets/cyclestreets-r")
library(sf)
library(stplanr)
library(cyclestreets)
desire_lines = od::od_to_sf(od::od_data_df, od::od_data_zones)[4:7, 1:3]
routes_stplanr = route(l = desire_lines, route_fun = journey, plan = "quietest")
plot(routes_stplanr$geometry)
desire_lines$id = 1:nrow(desire_lines)
routes_batch = batch(desire_lines, username = "robinlovelace", wait_time = 30)
# routes_batch = batch(desire_lines, username = "robinlovelace", wait_time = 30, id = 218)
plot(routes_batch$geometry)
identical(routes_stplanr$geometry, routes_batch$geometry)
routes_stplanr
routes_batch
names(routes_stplanr)
names(routes_batch)
waldo::compare(names(routes_stplanr), names(routes_batch))
waldo::compare(routes_stplanr, routes_batch)



nrows_batch = table(routes_batch$id)
df_batch = sf::st_drop_geometry(routes_batch)
inds = rep(seq(nrow(desire_lines)), times = nrows_batch)
df_routes_expanded = sf::st_drop_geometry(desire_lines)[inds, ]
df_batch = cbind(df_routes_expanded, df_batch)
routes_batch_updated = sf::st_sf(df_batch, geometry = routes_batch$geometry)
waldo::compare(names(routes_stplanr), names(routes_batch_updated))
setdiff(names(routes_stplanr), names(routes_batch_updated))
setdiff(names(routes_batch_updated), names(routes_stplanr))
