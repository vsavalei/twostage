
.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion("twostage")
  packageStartupMessage("This is twostage, early development version ", version, ".\nPlease report any bugs or uninformative error messages.")
}
