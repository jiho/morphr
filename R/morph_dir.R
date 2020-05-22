#' Morph images from a directory into one
#'
#' @param path path to the directory containing image files to morph together
#' @param n how many images to randomly select within the directory. Set this to a very high number to process all
#' @param write boolean, whether to write the image next to the directory in which the files are read. When FALSE (the default), the morphed image is returned, as a matrix. When TRUE, the image is written to disk and returned invisibly by the function.
#' @inheritParams morph
#' @return A matrix containing the pixel values for the morphed image, in `[0,255]` (black to white).
#' @export
#' @examples
#' amph <- system.file("extdata", "amphipoda", package="morphr")
#' imshow(morph_dir(amph))
#' imshow(morph_dir(amph, n=3))
#' imshow(morph_dir(amph, n=10, adjust_grey=TRUE))
morph_dir <- function(path, n=15, write=FALSE, adjust_grey=FALSE) {
  img <- pymorph$morph_dir(path, as.integer(n), write, adjust_grey)
  if (write) {
    invisible(img)
  } else {
    img
  }
}
