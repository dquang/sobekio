.onAttach <- function(libname, pkgname) {
  packageStartupMessage("sobekio version: ",
                        packageVersion('sobekio'),
                        '. Contact: 5876'
                        )
}
