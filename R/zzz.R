## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Initializes all package options on package loading
.onLoad <- function(libname, pkgname) {
  # Initializes global option list for keeping registered components
  options(shinybound.components = list())
}
