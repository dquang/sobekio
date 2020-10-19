#' Unload the DLL file
#' @noRd
.onUnload <- function(libpath=file.path(.libPaths(), "sobekio")) {
  library.dynam.unload("sobekio", libpath = libpath)
}

## usethis namespace: start
#' @useDynLib sobekio, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL
