#' Make a greyscale image transparent
#'
#' Turn white into fully transparent and black into fully opaque in a greyscale image.
#'
#' @param x input greyscale image, of type [imager::cimg()].
#'
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "16195419.jpg", package="morphr"))
#' img_show(x)
#' img_show(make_transparent(x))
make_transparent <- function(x) {
  # create pure black RGBA array
  rgba <- array(data=0, dim=c(imager::width(x), imager::height(x), 1, 4))

  # replace RGB by the actual image
  rgba[,,1,1:3] <- x[,,1,1]

  # turn the input image into a transparency mask
  rgba[,,1,4] <- 1-x[,,1,1]

  # convert back into a cimg object
  rgba <- as.cimg(rgba)

  return(rgba)
}
