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

  # instead of padding on each side, just create an empty image and paste the current image at the correct location within it (much faster)

  # create and empty image with the fill colour
  xf <- imager::imfill(x=w, y=h, val=col)

  # define corner point in width and height
  dims <- dim(x)
  cx <- max(0, round(w/2 - around[1]))
  cy <- max(0, round(h/2 - around[2]))
  # quick dimension checks
  if ((cx+dims[1]) > w | (cy+dims[2]) > h) {
    stop("Target dimensions w or h too small to center image")
  }

  # paste the image content there
  xf[(cx+1):(cx+dims[1]),(cy+1):(cy+dims[2]),1,1] = x[,,1,1]

  return(xf)
}
