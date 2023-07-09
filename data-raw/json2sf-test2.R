f = system.file(package = "cyclestreets", "extdata/journey.json")
obj = jsonlite::read_json(f, simplifyVector = TRUE)
rsf = json2sf_cs(obj, cols = c("distances"))
bench::mark(
  test = json2sf_cs(obj, cols = c("distances"))
)
# 90 itr/sec # Around 30 itr/sec for typical commute routes
f = function() {
  f = system.file(package = "cyclestreets", "extdata/journey.json")
  obj = jsonlite::read_json(f, simplifyVector = TRUE)
  json2sf_cs(obj, cols = c("distances"))
}
profvis::profvis(f())

devtools::load_all()
profvis::profvis(batch_read("test-data-7mb.csv"))
