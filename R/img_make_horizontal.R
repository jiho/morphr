#' Rotate object on image horizontally
#'
#' Measure the orientation angle with [img_orientation()], rotate the image, then use [img_centroid()] to detect the centroid and flip the image so that the centroid is is the top-right quadrant. All this works with a light-on-dark image.
#'
#' @inheritParams img_write
#'
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "amphipoda/33463695.jpg",
#'              package="morphr"), invert=TRUE) %>% img_show()
#' img_make_horizontal(x) %>% img_show()
img_make_horizontal <- function(x) {
  # rotate image to make it horizontal
  angle <- img_orientation(x)
  x <- imager::imrotate(x, -angle)
  # put the centroid it in the top right quadrant
  centroid <- img_centroid(x)
  w <- imager::width(x)
  h <- imager::height(x)
  if (centroid[1] < w/2) {
    x <- imager::mirror(x, "x")
  }
  if (centroid[2] > h/2) {
    # NB: the y axis is increasing when going down!
    x <- imager::mirror(x, "y")
  }
  return(x)
}