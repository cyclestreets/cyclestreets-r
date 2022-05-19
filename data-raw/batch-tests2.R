library(sf)
library(stplanr)
desire_lines = od::od_to_sf(od::od_data_df, od::od_data_zones)[4:5, 1:3]
routes_stplanr = route(l = desire_lines, route_fun = journey, plan = "quietest")
plot(routes_stplanr$geometry[1])
desire_lines$id = 1:nrow(desire_lines)
routes_batch = batch(desire_lines, username = "robinlovelace", wait_time = 30)
# routes_batch = batch(desire_lines, username = "robinlovelace", wait_time = 30, id = 218)
plot(routes_batch$geometry[1])
identical(routes_stplanr$geometry, routes_batch$geometry)
names(routes_stplanr)[-c(1:4)]
names(routes_batch)
waldo::compare(names(routes_stplanr), names(routes_batch))

nrows_batch = table(routes_batch$id)
df_batch = sf::st_drop_geometry(routes_batch)
inds = rep(seq(nrow(desire_lines)), times = nrows_batch)
df_routes_expanded = sf::st_drop_geometry(desire_lines)[inds, ]
df_batch = cbind(df_routes_expanded, df_batch)
routes_batch_updated = sf::st_sf(df_batch, geometry = routes_batch$geometry)
waldo::compare(names(routes_stplanr), names(routes_batch_updated))
setdiff(names(routes_stplanr), names(routes_batch_updated))
setdiff(names(routes_batch_updated), names(routes_stplanr))
