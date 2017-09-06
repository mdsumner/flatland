#' read fun
#'
#'  Details
#' @importFrom dplyr %>%
#' @importFrom raster brick stack
#' @export
read_cd_var <- function(date, xylim = NULL, varname = "ugosa", ..., latest = TRUE, return_files = FALSE) {
  files <- raadfiles::altimetry_daily_files()
  if (missing(date)) date <- NULL
  files <- .process_files(date, files, "daily", latest = latest)
  if (return_files) return(files)

  ## now commit to just the subset files
  files <- dplyr::filter(files, use_file)
  nfiles <- nrow(files)
 pb <- flat_progress(nfiles)
  pb$tick(0)

  msk <- NULL
  rot <- FALSE
  files$band <- 1
  op <- options(warn = -1)
  r0 <- raster::brick(raster::stack(lapply(seq_len(nrow(files)), function(xi)
    read_raster_progress(files$fullname[xi], ext = xylim, msk = msk, rot = rot, varname = varname, band = files$band[xi]))), progress = pb, ...)
  options(op)
  r0 <- setZ(r0, files$date)
  r0
}

axis_transforms <- function(x) {
  nc <- tidync::tidync(x)
  axes <- tidync:::axis_transforms.default(nc)
  axes
}
hypertidy <- function(x, ...) {
  axes0 <- lapply(x$fullname, axis_transforms)
  axes <- axes0[[1]]
  axes$time <- dplyr::bind_rows(lapply(axes0, function(a) a[["time"]]))
  list(source = x, transforms = axes)
}
aa <- hypertidy(raadfiles::altimetry_daily_files())
