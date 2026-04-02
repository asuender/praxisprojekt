library(here)
library(checkmate)

#' Download configured raw datasets
#'
#' Iterates over \code{config.dataset.urls}, validates each entry, and
#' downloads files into \code{data/raw/} when they are not present yet.
#'
#' @return Invisibly returns \code{NULL}.
download_data <- function() {
  assert_list(config.dataset.urls, types = "list", min.len = 1)

  lapply(config.dataset.urls, function(dataset) {
    assert_names(names(dataset), must.include = c("url", "filename"))
    assert_string(dataset$url, min.chars = 1)
    assert_string(dataset$filename, min.chars = 1)

    dest <- here("data", "raw", dataset$filename)

    if (file.exists(dest)) {
      message("Skipping '", dataset$filename, "' (already exists)")
      return(invisible(NULL))
    }

    message("Downloading '", dataset$filename, "' ...")
    download.file(dataset$url, dest, mode = "wb")
  })

  invisible(NULL)
}
