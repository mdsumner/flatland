#' read_raster0 <- function(x, varname) raster::raster(x)

read_raster_progress <- function(xfile, ext, msk, rot, varname = "", band = 1, progress) {
  pb$tick()
  mask_if_needed(crop_if_needed(rotate_if_needed(raster::raster(xfile, varname = varname, band = band), rot), ext), msk)
}
