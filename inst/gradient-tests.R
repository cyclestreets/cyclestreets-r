# Aim: test gradient calculations in CycleStreets

remotes::install_github("joeytalbot/cyclestreets")
library(cyclestreets)

from = tmaptools::geocode_OSM("potternewton park")
to = tmaptools::geocode_OSM("university of leeds")
r = journey(from$coords, to$coords, cols = NULL, cols_extra = NULL)
mapview::mapview(r["gradient_segment"])


# single route ------------------------------------------------------------

# smooth unwanted high gradients
summary(r$distances)
summary(r$gradient_segment)
plot(r$distances, r$gradient_segment)

distance_cutoff = 20
gradient_cutoff = 0.1
sel = r$gradient_segment > 0.1 &
  r$distances <= distance_cutoff
summary(sel)
r$gradient_segment_smooth = stplanr::route_rolling_average(r$gradient_segment)
r$gradient_segment[sel]
r$gradient_segment_smooth[sel]

# r$gradient_segment[sel] = r$gradient_segment_smooth[sel]

plot(r$distances, r$gradient_segment)
mapview::mapview(r["gradient_segment"])

smooth_with_cutoffs = function(
  gradient_segment,
  distances,
  distance_cutoff = 20,
  gradient_cutoff = 0.1
  ) {
  sel = gradient_segment > 0.1 &
    distances <= distance_cutoff
  summary(sel)
  gradient_segment_smooth = stplanr::route_rolling_average(gradient_segment)
  gradient_segment[sel]
  gradient_segment_smooth[sel]

  gradient_segment[sel] = gradient_segment_smooth[sel]
  gradient_segment

}

r$gradient_segment
smooth_with_cutoffs(gradient_segment = r$gradient_segment, distances = r$distances)

# chapeltown dataset ------------------------------------------------------

library(stplanr)
l = od2line(od_data_sample, cents_sf)
r = route(l = l, route_fun = cyclestreets::journey, smooth_gradient = TRUE, distance_cutoff = 50)
nrow(l)
nrow(r)
length(unique(r$route_number)) # omits routes with zero length
plot(r)
mapview::mapview(r["gradient_smooth"])
mapview::mapview(r[r$gradient_smooth > 0.1, "gradient_smooth"])
summary(r$distances[r$gradient_smooth > 0.1])

# with Lisbon data
u = "http://web.tecnico.ulisboa.pt/~rosamfelix/gis/declives/RedeViaria_Lisboa_Declives.rar"
d = tempdir()
f = file.path(d, "declives.rar")
download.file(u, f)
archive::archive(f)
# archive::archive_extract(f, dir = d, file = "RedeViariaDeclives.shp")
library(dplyr)
red = sf::read_sf("~/wip/pctLisbon-data/RedeViariaDeclives.shp")
plot(red %>% select(matches("dec")))
