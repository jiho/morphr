#' Guess the background colour of an image
#'
#' Guess the background colour as the dominant colour at the border of the image.
#'
#' @inheritParams img_write
#'
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "blob.jpg", package="morphr")) %>% img_show()
#' img_guess_background(x)
#' x <- img_read(system.file("extdata", "shape.png", package="morphr")) %>% img_show()
#' img_guess_background(x)
img_guess_background <- function(x) {
  stats::median(c(x[1,], x[,1]))
}