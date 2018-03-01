#' Morph images from a directory into one
#'
#' @param path path to the directory containing image files to morph together
#' @param n how many images to randomly select within the directory. Set this to a very high number to process all
#' @param write boolean, whether to write the image next to the directiry in which the files are read. When FALSE (the default), the morphed image is returned, as a matrix. When TRUE, the image is written to disk and returned invisibly by the function.
#' @return A matrix containing the pixel values for the morphed image, in [0,255] (black to white).
#' @export
#' @examples
#' img <- morph_dir(system.file("extdata", "amphipoda", package="morphr"))
#' image(img, col=grey(1:255/255), asp=1)
#' img <- morph_dir(system.file("extdata", "amphipoda", package="morphr"), n=3)
#' image(img, col=grey(1:255/255), asp=1)
morph_dir <- function(path, n=15, write=FALSE) {
  img <- pymorph$morph_dir(path, as.integer(n), write)
  if (write) {
    invisible(img)
  } else {
    img
  }
}
