#' Common progress bar for flatland read functions
flat_progress <-  function(nn, ...) {
  progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nn, clear = FALSE, width= 60)
}


#' Resolve input dates to source dates
#'
#' @importFrom dplyr %>%  mutate
.process_files <-
  function(dt = NULL, fls, tr, latest = TRUE) {
    file_dates <- fls[["date"]]
    if (is.null(dt)) {
      date <- if (latest) {max(file_dates)} else {min(file_dates)}
    } else {
      date <- timedateFrom(dt)
    }
    findex <- .process_dates(date, file_dates, tr)
    fls %>% dplyr::mutate(use_file = seq_len(nrow(fls)) %in% findex)
  }

