#' Read a grayscale image
#'
#' @param file path to the input image file.
#'
#' @return A matrix with the pixel values of the image. Indexes are counted from the top left (i.e. like a matrix).
#' @export
#' @examples
#' path <- system.file("extdata", "16195419.jpg", package="morphr")
#' x <- img_read(path)
#' img_show(x)
img_read <- function(file) {
  x <- imager::load.image(file)
  # drop colour depth and colour channel info
  # TODO support colour images
  x <- x[,,1,1]
  return(x)
}