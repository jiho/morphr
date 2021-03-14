#' Center an object in an image
#'
#' Center the input object within an output image of given dimensions, around a set of coordinates (by default the centroid of the object).
#'
#' @inheritParams img_write
#' @param w,h the width and height of the output image.
#' @param around a vector with the x,y coordinate of the point around which to center; by default the centroid of the input image.
#' @param col the colour to use for padding; by default the background colour from [img_guess_background()].
#'
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "amphipoda/33463695.jpg",
#'               package="morphr"), invert=TRUE)
#' img_show(x)
#' # center with all defaults
#' # = around the center of mass of grey levels
#' xc <- img_center(x, 350, 350) %>% img_show()
#' # check that the new centroid is around the middle of the image [175,175]
#' img_centroid(xc)
#'
#' # center around the centroid of the binary image
#' # = does not take grey levels into account
#' xc <- img_center(x, 350, 350, around=img_centroid(x>0)) %>% img_show()
#' img_centroid(xc>0)
#' # the binary centroid is in the middle
#' img_centroid(xc)
#' # but the grey levels center of mass is elsewhere
img_center <- function(x, w, h, around=NULL, col=NULL) {
  # define point to center around
  if (is.null(around)) {
    around <- img_centroid(x)
  }
  # define padding colour
  if (is.null(col)) {
    col <- img_guess_background(x)
  }

  # Function to define padding length in one dimension
  pads <- function(length, centroid, target) {
    before <- round(target/2 - centroid)
    after <- target - (before + length)
    if (after < 0) {
      stop("Target dimension too small")
    }
    return(c(before, after))
  }
  # define padding on width and height
  dims <- dim(x)
  padw <- pads(dims[1], around[1], w)
  padh <- pads(dims[2], around[2], h)

  # perform the padding
  x %>%
    imager::pad(padw[1], axes="x", pos=-1, val=col) %>%
    imager::pad(padw[2], axes="x", pos= 1, val=col) %>%
    imager::pad(padh[1], axes="y", pos=-1, val=col) %>%
    imager::pad(padh[2], axes="y", pos= 1, val=col)
}