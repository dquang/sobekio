.onAttach <- function(libname, pkgname) {
  packageStartupMessage("sobekio version: ", packageVersion('sobekio'),
  '. Repo: https://github.com/dquang/sobekio')
}