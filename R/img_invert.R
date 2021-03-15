#' Invert a greyscale image
#'
#' This is a simple 1-x operation but having it as a function ease its inclusing in within a piped chain of commands
#'
#' @inheritParams img_write
#'
#' @return The inverted image, as a [imager::cimg()] object.
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "blob.jpg", package="morphr"))
#' img_show(x)
#' img_show(img_invert(x))
img_invert <- function(x) {
  1 - x
}
