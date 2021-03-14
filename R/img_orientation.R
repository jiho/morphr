#' Orientation of object on image
#'
#' Compute the orientation angle of the best fitting ellipse based on image moments.
#'
#' @inheritParams img_make_transparent
#' @param direction try to resolve the 180º uncertainty in orientation: orient the ellipse (and therefore the angle) towards the denser part of the object. On a binary image, the denser part is the largest. On a greyscale image, the denser part is the lightest (because black=0, white=1).
#'
#' @details Image moments only make sense for light on dark images. If your image is dark on light, invert it before computing the orientation with this function.
#'
#' On an image displayed with [img_show()], the angle is given from the horizontal, in clockwise direction, in degrees. The computation gives it in trigonometric convention (from the horizontal, towards the positive side of the y axis, in radians) but (i) in an image, the y axis increases when going *down*, (ii) the angle is converted in degrees for easiness.
#'
#' @return The orientation angle.
#' @export
#' @examples
#' # read an image of an asymetric shape
#' x <- img_read(system.file("extdata", "shape.png", package="morphr"))
#' x <- imager::imrotate(x, 45)
#' img_show(x)
#' img_orientation(x)
#' # 45º from horizontal, clockwise
#' img_orientation(x, direction=TRUE)
#' # the highest density is in the same direction
#' # rotate it to make the object horizontal, with highest density on the right
#' imager::imrotate(x, -45) %>% img_show()
#'
#' # now if we work with the mask of the object
#' img_show(x>0)
#' img_orientation(x>0)
#' img_orientation(x>0, direction=TRUE)
#' # the highest density is now at the opposite end
#' # so the angles are opposed by 180º
#' # and to make the object horizontal and with highest density on the right
#' imager::imrotate(x>0, -225) %>% img_show()
#'
#' # try with a real image
#' # (NB: inverted to allow the computation of moments)
#' x <- img_read(system.file("extdata", "amphipoda/33463695.jpg",
#'               package="morphr"), invert=TRUE)
#' img_show(x)
#' a <- img_orientation(x, direction=TRUE)
#' imager::imrotate(x, -a) %>% img_show()
img_orientation <- function(x, direction=FALSE) {
  # http://raphael.candelier.fr/?blog=Image%20Moments

  # compute orientation
  mu <- img_moments_central(x, order=2)
  orient <- 0.5 * atan(2 * gm(mu,1,1) / (gm(mu,2,0) - gm(mu,0,2))) + (gm(mu,2,0) < gm(mu,0,2)) * pi/2
  orient <- orient * 180 / pi
  # make it in [0, 180]
  if (orient < 0) {orient <- 180 + orient}

  # try to give direction the object
  if (direction) {
    # rotate the image based on the orientation above
    r <- imager::imrotate(x, -orient)
    # compute center and centroid
    x_center <- dim(r)[1]/2
    x_centroid <- img_centroid(r)[1]
    # if the centroid is on the left of the center,
    # put it on the right
    if (x_centroid < x_center) {
      orient <- orient + 180
    }
  }
  return(orient)
}
