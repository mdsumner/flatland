

library(raadtools)
files <- raadfiles::oisst_daily_files()

## a combination of raadtools and lazyraster and gdalcubes
oisst <- structure(list(files_ = files,
                        active_source_ = files$fullname[1L],
                        active_variable_ = tail(strsplit(vapour::vapour_sds_names(files$fullname[1])$subdataset[1], ":")[[1]], 1),
                        reader_ = lazyraster::lazyraster(files$fullname[1L]),
                        template_ = raster::raster(raster::raster(files$fullname[1L])),
                        variable_ = vapour::vapour_sds_names(files$fullname[1])),
                   class = "edwin")

fl_variables <- function(x) {
  unlist(lapply(strsplit(x$variable_$subdataset, ":"), tail, 1))
}
print.edwin <- function(x, ...) {
  print(x$template_)
  print(x$active_variable_)
  print(paste(fl_variables(x), collapse = ", "))
}
print(oisst)



