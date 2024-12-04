# cyclestreets 1.0.3 (2024-12)

* Increased timeout time (#91)
* Use retry

# cyclestreets 1.0.2 (2024-09)

* Minor update to fix an issue with the `batch()` function (#91)

# cyclestreets 1.0.1

* Bug fix: issue with batch identified and fix (#80) thanks to @mem48

# cyclestreets 1.0.0

* Breaking changes: there are some differences in inputs and outputs, check you code before updating
* Refactored to use faster implemenations throughout, thanks to Malcolm Morgan (@mem48) for SIMDJSON implementations
* You can now choose which columns to return with `cols_to_keep` in `journey()` and `batch()` functions, with minimal defaults for `batch()`
* Tests now pass

# cyclestreets 0.6.0 (February 2023)

* Experimental `journey2()` function added (#31)
* Documentation updated
* `batch()` is now less chatty

# cyclestreets 0.5.4 (January 2023)

* Batch routing implemented
* Remove unused field (#43)

# cyclestreets 0.5.3 (January 2022)

* Fixed issue with returning multiple identical routes when route consists of a single segment

# cyclestreets 0.5.2 (November 2021)

* No longer depends on `stplanr` for tests to pass, new internal function

# cyclestreets 0.5.1 (November 2021)

* Updated docs

# cyclestreets 0.5.0 (September 2021)

* `ltns()` function added

# cyclestreets 0.4.0 (June 2021)

* `quietness_segment` variable removed

# cyclestreets 0.3.0

* New functionality to estimate gradients
* Updated `json2sf_cs()` function provides more columns
* All route-level data now returned by default 

# cyclestreets 0.1.5

* Update package description, title and other things in preparation for CRAN submission.

# cyclestreets 0.1.1

* New argument cols added to `journey()` function allowing control of the output.
* Many more columns can now be accessed as illustrated by new example: `journey(from, to, cols = NULL)`.

# cyclestreets 0.1.0

* Package created as part of the R for Transport Applications course at the University of Leeds.
