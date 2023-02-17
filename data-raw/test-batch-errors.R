u = "https://github.com/cyclestreets/cyclestreets-r/files/8853002/test-data.csv.gz"
f = basename(u)

download.file(u, f)
devtools::load_all()
batch_read(f)
