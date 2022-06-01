# Aim: debug batch routing


devtools::load_all()
u = "https://github.com/cyclestreets/cyclestreets-r/files/8812620/test-data-batch.csv.gz"
f = basename(u)
download.file(url = u, destfile = f)
routes = batch_read(file = f)
plot(routes$geometry)
names(routes)
