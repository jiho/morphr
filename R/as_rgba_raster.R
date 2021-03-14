# Quick fix for the fact that imager objects are not well converted to raster when they have RGBA channels
as_rgba_raster <- function(x) {
  # split into channels
  ch <- imager::channels(x)
  # convert to colour
  xr <- grDevices::rgb(ch[[1]], ch[[2]], ch[[3]], ch[[4]])
  # wrap into a raster
  dim(xr) <- dim(x)[2:1]
  class(xr) <- "raster"
  return(xr)
}
