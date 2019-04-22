#' Morph several images into one
#'
#' @param paths paths to image files to morph together
#' @param dest path to a new file in which to write the morphed image. When empty (the default), the morphed image is returned as a matrix. When not empty, the image is written to disk and returned invisibly by the function.
#' @param adjust_grey whether to adjust the mean grey level of the morphed image to match that of the orignal images it was constructed with. Usually, computing the average(i.e. the morphing) results in lighter images; this compensates it a little. Note that it requires more intense computatin and is therefore switched off by default.
#' @return A matrix containing the pixel values for the morphed image, in [0,255] (black to white).
#' @export
#' @examples
#' amph <- list.files(
#'           system.file("extdata", "amphipoda", package="morphr"),
#'           full.names=TRUE
#'        )
#' img <- morph(amph)
#' imshow(img)
#' img <- morph(amph, adjust_grey=TRUE)
#' imshow(img)
#' \dontrun{
#' img <- morph(files, dest=path.expand("~/amphipoda_morphed.jpg"))
#' }
#' cres <- list.files(
#'           system.file("extdata", "creseidae", package="morphr"),
#'           full.names=TRUE
#'        )
#' img <- morph(cres)
#' imshow(img)
#' img <- morph(cres, adjust_grey=TRUE)
#' imshow(img)
morph <- function(paths, dest="", adjust_grey=FALSE) {
  # make sure paths is considered as a list even if it has only one element
  if ( length(paths) == 1 ) { paths <- list(paths) }
  img <- pymorph$morph(paths, dest, adjust_grey)
  if (dest != "") {
    invisible(img)
  } else {
    img
  }
}
