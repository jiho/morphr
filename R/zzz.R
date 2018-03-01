pymorph <- NULL
.onLoad <- function(libname, pkgname) {
  pymorph <<- reticulate::import_from_path("morph", path=system.file("python", package="morphr"), delay_load=TRUE)
}
