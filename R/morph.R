#' Morph several images into one
#'
#' @param paths paths to image files to morph together
#' @param dest path to a new file in which to write the morphed image. When empty (the default), the morphed image is returned as a matrix. When not empty, the image is written to disk and returned invisibly by the function.
#' @return A matrix containing the pixel values for the morphed image, in [0,255] (black to white).
#' @export
#' @examples
#' source <- system.file("extdata", "amphipoda", package="morphr")
#' files <- list.files(source, full.names=TRUE)
#' img <- morph(files)
#' image(img, col=grey(1:255/255), asp=1)
#' \dontrun{
#' img <- morph(files, dest=path.expand("~/amphipoda_morphed.jpg"))
#' }
morph <- function(paths, dest="") {
  img <- pymorph$morph(paths, dest)
  if (dest != "") {
    invisible(img)
  } else {
    img
  }
}
