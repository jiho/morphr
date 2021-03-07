#' Read a greyscale image
#'
#' @param file path to the input image file.
#'
#' @return A [imager::cimg()] object, which is a four-dimensional numeric array containing x,y,z,c coordinates, where x,y are dimensions, z is depth, and c is colour.
#' @export
#' @examples
#' path <- system.file("extdata", "16195419.jpg", package="morphr")
#' x <- img_read(path)
#' img_show(x)
img_read <- function(file) {
  im <- imager::load.image(file)
  # keep only one colour channel
  # TODO support colour images
  return(im[,,,1,drop=FALSE])
}