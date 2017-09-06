
.determine.time.resolution <- function(x, ...) {
  rng <- range(difftime(x[-1L], x[-length(x)], units = "days"))
  a <- round(min(rng))
  if (a == 0) {
    a <- round(24 * as.numeric(min(rng)))
    return(sprintf("%shourly", a))
  }
  if (a == 1) {
    return("daily")
  }
  if (a %in% 5:9) {
    val = "weekly"
  } else {
    val = "monthly"
  }
  val

}



.interp <- function(x1, x2, proportion) {
  x1 * (1 - proportion) + x2 * proportion
}

.calcProportion <- function(xmin, xmax, x) {
  (unclass(x) - unclass(xmin) ) / (unclass(xmax) - unclass(xmin))
}


.big_extract <-  function (x, y,  ctstime = FALSE, fact = NULL, verbose = TRUE, ...) {
  result <- rep(as.numeric(NA), nrow(y))
  ## progress
  pb <- progress::progress_bar$new(
    format = "getting ready                  [:bar] :percent in :elapsed",
    total = 10, clear = FALSE, width= 80)
  pb$tick(0) ## ---------------------------------------------

  resize <- FALSE

  if (!is.null(fact)) resize <- TRUE
  notime <- FALSE
  pb$tick() ## ---------------------------------------------
  ## TODO, will have to figure out how to do this
  args <- list(...)
  if ("xylim" %in% names(args)) {
    warning("xylim argument ignored (determined automatically from the input data)")
    args$xylim <- NULL
  }
  pb$tick() ## ---------------------------------------------
  if ("time.resolution" %in% names(args)) {
    files <- x(return_files = TRUE, time.resolution = args$time.resolution, ...)
  } else {
    files <- x(return_files = TRUE, ...)
  }
  if (length(files) == 1L) {
    notime <- TRUE
  }
  pb$tick() ## ---------------------------------------------
  ## data.frame input has  assumed structure
  ## we assume y is lon,lat,time
  y1 <- SpatialPoints(as.matrix(y[,1:2]), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  pb$tick() ## ---------------------------------------------
  if (notime) {
    ## assume we want topo/bathy values
    thisx1 <- x(xylim = xylim, ...)
    if (resize) thisx1 <- aggregate(thisx1, fact = fact, fun = 'mean')
    return(extract(thisx1, y1, ...))
  }
  pb$tick() ## ---------------------------------------------
  times <- try(timedateFrom(y[[3L]]))
  y <- y1
  ## chuck a
  if (inherits(times, "try-error") | any(is.na(times))) {
    ##.standard.assumeXYT.TimeError()
  }




  pb$tick() ## ---------------------------------------------

  dummy <- x(inputfiles = files, ...)
  yp <- spTransform(y1, projection(dummy))
  pb$tick() ## ---------------------------------------------
  xylim <- extent(yp)
  dx <- xmax(xylim)-xmin(xylim)
  dy <- ymax(xylim)-ymin(xylim)
  xylim <- xylim + c(dx, dy) / 10
  pb$tick() ## ---------------------------------------------

  ## TODO, this is awful need a fix
  time.resolution <- .determine.time.resolution(files$date)
  ## TODO somehow manage climatology exceptions
  ## unique indexes
  pb$tick() ## ---------------------------------------------
  findex <- suppressWarnings(.processDates(times, files$date, timeres = time.resolution))
  windex <- .indexDates(times, files$date)
  pb$tick() ## ---------------------------------------------
  ## this won't always work, need to zap anything out of range . . .
  if (max(times) == max(files$date[findex])) findex <- c(findex, max(findex) + 1)
  findex <- findex[findex <= nrow(files)]
  date <- files$date[findex]
  l <- list(...)
  if ("inputfiles" %in% names(l)) warning("using inputfiles explicitly is deprecated, please don't do it")
  mess1 <- ""
  pb$tick() ## ---------------------------------------------
  ## progress
  pb <- progress::progress_bar$new(
    format = "extracting :what file :ith of :nn [:bar] :percent in :elapsed",
    total = length(date), clear = FALSE, width= 80)
  pb$tick(0, tokens = list(what = time.resolution, ith = 1, nn = length(date)))
  for (i in seq_along(date)) {
    thisx <- x(date[i], verbose = FALSE, inputfiles = files, xylim = xylim,  ...)

    if(resize) thisx <- aggregate(thisx, fact = fact, fun = "mean")
    asub <- windex == findex[i]
    ## no interpolation in time, controlled by "method" for xy
    if (any(asub)) {result[asub] <- suppressWarnings(extract(thisx, y[asub, ], ...))}

    if (interactive() & verbose) {
      pb$tick(tokens = list(what = time.resolution, ith = i, nn = length(date)))

    }
  }

  result

}

