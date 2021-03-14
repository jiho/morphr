#' Chop away borders from an image
#'
#' @param x input image (a [imager::cimg()] object).
#' @param top number of pixels to chop from the top.
#' @param right same, from the right.
#' @param bottom same, from the bottom
#' @param left same, from the left.
#'
#' @return The image with the pixels chopped off.
#'
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "blob.jpg", package="morphr"))
#' img_show(x)
#' # chop off some pieces around the main object
#' img_chop(x, t=17, r=15, b=50, l=15) %>% img_show()
#' # just remove the legend
#' img_chop(x, b=31) %>% img_show()
img_chop <- function(x, top=0, right=0, bottom=0, left=0) {
  w <- imager::width(x)
  h <- imager::height(x)
  x[(1+left):(w-right),(1+top):(h-bottom),,,drop=FALSE]
}
